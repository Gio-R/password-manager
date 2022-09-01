package is.clipperz.backend.services

import java.io.{ File, FileOutputStream, IOException }
import java.nio.file.Path
import java.security.MessageDigest
import zio.{ ZIO, ZLayer, Task, Chunk }
import zio.stream.{ ZStream, ZSink }
import zio.json.{ JsonDecoder, JsonEncoder, DeriveJsonDecoder, DeriveJsonEncoder }
import is.clipperz.backend.data.HexString
import is.clipperz.backend.data.HexString.{ bytesToHex }
import is.clipperz.backend.functions.crypto.HashFunction

// ----------------------------------------------------------------------------

type BlobHash = HexString

case class SaveBlobData(
  data: HexString,
  hash: BlobHash
)

object SaveBlobData:
  implicit val decoder: JsonDecoder[SaveBlobData] = DeriveJsonDecoder.gen[SaveBlobData]
  implicit val encoder: JsonEncoder[SaveBlobData] = DeriveJsonEncoder.gen[SaveBlobData]

// ----------------------------------------------------------------------------

trait BlobArchive:
  def getBlob(hash: BlobHash): Task[ZStream[Any, Throwable, Byte]]
  def saveBlob(key: BlobHash, content: ZStream[Any, Throwable, Byte]): Task[BlobHash]
  def deleteBlob(content: ZStream[Any, Throwable, Byte]): Task[Unit]

object BlobArchive:
  case class FileSystemBlobArchive(keyBlobArchive: KeyBlobArchive, tmpDir: Path) extends BlobArchive:
    override def getBlob(hash: BlobHash): Task[ZStream[Any, Throwable, Byte]] =
      keyBlobArchive.getBlob(hash.toString)

    override def saveBlob(key: BlobHash, content: ZStream[Any, Throwable, Byte]): Task[BlobHash] =
      val tmpFile = File.createTempFile("pre", "suff", tmpDir.toFile())
      ZIO.scoped {
        content
          .tapSink(ZSink.fromOutputStream(new FileOutputStream(tmpFile)))
          .run(ZSink.digest(MessageDigest.getInstance("SHA-256").nn))
          .map((chunk: Chunk[Byte]) => HexString.bytesToHex(chunk.toArray))
      }.flatMap { hash =>
        if (hash == key)
          ZIO.scoped {
            keyBlobArchive
              .saveBlob(hash.toString, ZStream.fromPath(tmpFile.nn.toPath().nn))
              .map(_ => tmpFile.nn.delete())
              .map(_ => hash)
          }
        else
          ZIO.fail(new Exception(s"hash of content does not match with hash in request"))
      }

    override def deleteBlob(content: ZStream[Any, Throwable, Byte]): Task[Unit] =
      ZIO.scoped {
        HashFunction
          .hashSHA256(content)
          .map(hash => keyBlobArchive.deleteBlob(bytesToHex(hash).toString))        
      }

  // . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

  def initializeBlobArchive(basePath: Path): Task[Unit] =
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

  def fs(basePath: Path, levels: Int): ZLayer[Any, Throwable, BlobArchive] =
    val keyBlobArchive = new KeyBlobArchive.FileSystemKeyBlobArchive(basePath, levels)
    val baseTmpPath = basePath.resolve("tmp").nn

    ZLayer.scoped(
      initializeBlobArchive(baseTmpPath)
        .map(_ => FileSystemBlobArchive(keyBlobArchive, baseTmpPath))
    )