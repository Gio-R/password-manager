package is.clipperz.backend.services

import java.io.{ File, FileOutputStream, IOException }
import java.nio.file.Path
import java.security.MessageDigest
import zio.{ ZIO, ZLayer, Task, Chunk }
import zio.stream.{ ZStream, ZSink }
import zio.json.{ JsonDecoder, JsonEncoder, DeriveJsonDecoder, DeriveJsonEncoder, EncoderOps }
import is.clipperz.backend.data.HexString
import is.clipperz.backend.data.HexString.bytesToHex
import is.clipperz.backend.functions.crypto.HashFunction
import is.clipperz.backend.exceptions.EmptyContentException
import zio.Duration
import java.io.FileNotFoundException
import is.clipperz.backend.functions.fromStream
import is.clipperz.backend.exceptions.NonWritableArchiveException
import is.clipperz.backend.exceptions.NonAuthorizedException
import is.clipperz.backend.exceptions.BadRequestException
import java.nio.charset.StandardCharsets

case class OTPBlob(verifier: HexString, encryptedPassword: HexString)
object OTPBlob:
  implicit val decoder: JsonDecoder[OTPBlob] = DeriveJsonDecoder.gen[OTPBlob]
  implicit val encoder: JsonEncoder[OTPBlob] = DeriveJsonEncoder.gen[OTPBlob]

type OTPKey = HexString

// ------------------------

case class SaveOTPBlobData(key: OTPKey, otpBlob: OTPBlob)
object SaveOTPBlobData:
  implicit val decoder: JsonDecoder[SaveOTPBlobData] = DeriveJsonDecoder.gen[SaveOTPBlobData]
  implicit val encoder: JsonEncoder[SaveOTPBlobData] = DeriveJsonEncoder.gen[SaveOTPBlobData]

// ------------------------

trait OTPArchive:
  def getOTPBlob(hash: OTPKey, verifier: HexString): Task[OTPBlob]
  def saveOTPBlob(key: OTPKey, content: OTPBlob): Task[OTPKey]
  def deleteOTPBlob(key: OTPKey, content: OTPBlob): Task[Boolean]

object OTPArchive:
  val WAIT_TIME = 100

  case class FileSystemOTPArchive(keyBlobArchive: KeyBlobArchive, tmpDir: Path) extends OTPArchive:
    override def getOTPBlob(hash: OTPKey, verifier: HexString): Task[OTPBlob] =
      keyBlobArchive
        .getBlob(hash.toString)
        .flatMap(content =>
          fromStream[OTPBlob](content)
            .flatMap(bl => 
              keyBlobArchive
                .deleteBlob(hash.toString)
                .flatMap(_ => if bl.verifier == verifier then ZIO.succeed(bl) else ZIO.fail(new NonAuthorizedException("Sent verifier is not correct")))
            )
        )

    override def saveOTPBlob(key: OTPKey, content: OTPBlob): Task[OTPKey] =
      keyBlobArchive
          .saveBlob(
            key.toString(),
            ZStream.fromChunks(Chunk.fromArray(content.toJson.getBytes(StandardCharsets.UTF_8).nn)),
          )
          .map(_ => key)

    override def deleteOTPBlob(key: OTPKey, content: OTPBlob): Task[Boolean] =
      this
        .getOTPBlob(key, content.verifier)
        .flatMap(blob =>
          if blob == content then
            keyBlobArchive.deleteBlob(key.toString())
          else 
            ZIO.fail(new NonAuthorizedException("Saved blob is different than one sent"))  
        )
        .catchSome {
          case ex: NonAuthorizedException => ZIO.fail(new NonAuthorizedException("Saved blob is different than one sent"))
        }

  // . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  def initializeOTPArchive(basePath: Path): Task[Unit] =
    ZIO.attempt {
      val file = basePath.toFile()
      val tempFolderSuccessfullyCreated: Boolean =
        (file match
          case null => None
          case _ => Some(file)
        ).map(p => if p.exists() then true else p.mkdirs())
          .getOrElse(false)
      if (tempFolderSuccessfullyCreated == false)
        throw new IOException("Failed initialization of temporary blob directory")
    }

  def fs(
      basePath: Path,
      levels: Int,
      requireExistingPath: Boolean = true,
    ): ZLayer[Any, Throwable, OTPArchive] =
    val keyBlobArchive = KeyBlobArchive.FileSystemKeyBlobArchive(basePath, levels, requireExistingPath)
    val baseTmpPath = basePath.resolve("tmp").nn

    ZLayer.scoped(
      initializeOTPArchive(baseTmpPath)
        .map(_ => FileSystemOTPArchive(keyBlobArchive, baseTmpPath))
    )
