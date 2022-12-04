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
import java.util.Date
import java.time.Instant
import is.clipperz.backend.exceptions.FailedConversionException

case class OTPBlob(verifier: HexString, encryptedPassword: HexString)
object OTPBlob:
  implicit val decoder: JsonDecoder[OTPBlob] = DeriveJsonDecoder.gen[OTPBlob]
  implicit val encoder: JsonEncoder[OTPBlob] = DeriveJsonEncoder.gen[OTPBlob]

enum OTPUseCases:
  case USED, DISABLED
object OTPUseCases:
  implicit val decoder: JsonDecoder[OTPUseCases] = DeriveJsonDecoder.gen[OTPUseCases]
  implicit val encoder: JsonEncoder[OTPUseCases] = DeriveJsonEncoder.gen[OTPUseCases]

case class UsedOTPBlob(verifier: HexString, dateTime: Instant, useCase: OTPUseCases)
object UsedOTPBlob:
  implicit val decoder: JsonDecoder[UsedOTPBlob] = DeriveJsonDecoder.gen[UsedOTPBlob]
  implicit val encoder: JsonEncoder[UsedOTPBlob] = DeriveJsonEncoder.gen[UsedOTPBlob]

type OTPKey = HexString

// ------------------------

case class SaveOTPBlobData(key: OTPKey, otpBlob: OTPBlob)
object SaveOTPBlobData:
  implicit val decoder: JsonDecoder[SaveOTPBlobData] = DeriveJsonDecoder.gen[SaveOTPBlobData]
  implicit val encoder: JsonEncoder[SaveOTPBlobData] = DeriveJsonEncoder.gen[SaveOTPBlobData]

case class SaveUsedOTPBlobData(key: OTPKey, otpBlob: UsedOTPBlob)
object SaveUsedOTPBlobData:
  implicit val decoder: JsonDecoder[SaveUsedOTPBlobData] = DeriveJsonDecoder.gen[SaveUsedOTPBlobData]
  implicit val encoder: JsonEncoder[SaveUsedOTPBlobData] = DeriveJsonEncoder.gen[SaveUsedOTPBlobData]

// ------------------------

trait OTPArchive:
  def getOTPBlob(hash: OTPKey, verifier: HexString): Task[Either[UsedOTPBlob, OTPBlob]]
  def saveOTPBlob(key: OTPKey, content: OTPBlob): Task[OTPKey]
  def deleteOTPBlob(key: OTPKey, content: Either[UsedOTPBlob, OTPBlob]): Task[Boolean]

object OTPArchive:
  val WAIT_TIME = 100

  case class FileSystemOTPArchive(keyBlobArchive: KeyBlobArchive, tmpDir: Path) extends OTPArchive:
    override def getOTPBlob(hash: OTPKey, verifier: HexString): Task[Either[UsedOTPBlob, OTPBlob]] =
      keyBlobArchive
        .getBlob(hash.toString)
        .flatMap(content =>
          fromStream[UsedOTPBlob](content)
            .flatMap(usedBlob => 
              if usedBlob.verifier == verifier then
                ZIO.succeed(Left(usedBlob))
              else
                ZIO.fail(new NonAuthorizedException("Sent verifier is not correct"))
            )
            .catchSome {
              case ex: FailedConversionException =>
                fromStream[OTPBlob](content)
                  .flatMap(blob =>
                    if blob.verifier == verifier then 
                      val usedOTPBlob = UsedOTPBlob(blob.verifier, Instant.now().nn, OTPUseCases.USED)
                      keyBlobArchive
                        .saveBlob(hash.toString, ZStream.fromChunks(Chunk.fromArray(usedOTPBlob.toJson.getBytes(StandardCharsets.UTF_8).nn)))
                        .flatMap(_ => ZIO.succeed(Right(blob)))
                    else 
                      val usedOTPBlob = UsedOTPBlob(blob.verifier, Instant.now().nn, OTPUseCases.DISABLED)
                      keyBlobArchive
                        .saveBlob(hash.toString, ZStream.fromChunks(Chunk.fromArray(usedOTPBlob.toJson.getBytes(StandardCharsets.UTF_8).nn)))
                        .flatMap(_ => ZIO.fail(new NonAuthorizedException("Sent verifier is not correct")))  
                  )
            }
        )

    override def saveOTPBlob(key: OTPKey, content: OTPBlob): Task[OTPKey] =
      keyBlobArchive
          .saveBlob(
            key.toString(),
            ZStream.fromChunks(Chunk.fromArray(content.toJson.getBytes(StandardCharsets.UTF_8).nn)),
          )
          .map(_ => key)

    override def deleteOTPBlob(key: OTPKey, content: Either[UsedOTPBlob, OTPBlob]): Task[Boolean] =
      val verifier = content match
                      case Left(o) => o.verifier
                      case Right(o) => o.verifier
      this
        .getOTPBlob(key, verifier)
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
