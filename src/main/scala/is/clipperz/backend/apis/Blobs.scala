package is.clipperz.backend.apis

import java.io.FileNotFoundException
import zio.{ ZIO }
import zio.stream.{ ZStream }
import zhttp.http.{ Headers, HeaderNames, HeaderValues, Http, HttpData, Method, Path, PathSyntax, Response, Status }
import zhttp.http.* //TODO: fix How do you import `!!` and `/`?
import is.clipperz.backend.data.HexString
import is.clipperz.backend.exceptions.{NonWritableArchiveException, NonReadableArchiveException, FailedConversionException, ResourceNotFoundException}
import is.clipperz.backend.functions.{ fromStream }
import is.clipperz.backend.services.{ BlobArchive, SaveBlobData }
import is.clipperz.backend.Main.ClipperzHttpApp
import is.clipperz.backend.exceptions.EmptyContentException
import is.clipperz.backend.exceptions.BadRequestException

val blobsApi: ClipperzHttpApp = Http.collectZIO {
  case request @ Method.POST -> !! / "blobs" =>
    ZIO
      .service[BlobArchive]
      .zip(ZIO.succeed(request.bodyAsStream))
      .flatMap((archive, bytes) => 
        fromStream[SaveBlobData](bytes)
        .flatMap(saveData =>
          archive.saveBlob(saveData.hash, ZStream.fromIterable(saveData.data.toByteArray))
        )
      )
      .map(results => Response.text(s"${results}"))
      .catchSome {
        case ex : EmptyContentException => { println(ex); ZIO.succeed(Response(status = Status.BadRequest)) }
        case ex : BadRequestException => { println(ex); ZIO.succeed(Response(status = Status.BadRequest)) }
        case ex : NonWritableArchiveException => { println(ex); ZIO.succeed(Response(status = Status.InternalServerError)) }
        case ex : FailedConversionException => { println(ex); ZIO.succeed(Response(status = Status.BadRequest)) }
      }

      // .map(hash => Response.text(s"${hash}"))

  case request @ Method.DELETE -> !! / "blobs" / hash =>
    ZIO
    .service[BlobArchive]
    .zip(ZIO.succeed(request.bodyAsStream))
    .flatMap((archive, bytes) =>
      fromStream[SaveBlobData](bytes)
      .flatMap(saveData =>
          archive.deleteBlob(ZStream.fromIterable(saveData.data.toByteArray))
        )
      )
      .map(b => if b then Response.ok else Response(status = Status.NotFound))
      .catchSome {
        case ex : NonWritableArchiveException => { println(ex); ZIO.succeed(Response(status = Status.InternalServerError)) }
        case ex : FailedConversionException => { println(ex); ZIO.succeed(Response(status = Status.BadRequest)) }
      }.catchAll(ex => { println(ex); ZIO.succeed(Response(status = Status.InternalServerError))})

  case request @ Method.GET -> !! / "blobs" / hash =>
    ZIO
      .service[BlobArchive]
      .flatMap(archive => archive.getBlob(HexString(hash)))
      .map((bytes: ZStream[Any, Throwable, Byte]) =>
        Response(
          status = Status.Ok,
          data = HttpData.fromStream(bytes),
          headers = Headers(HeaderNames.contentType, HeaderValues.applicationOctetStream),
        )
      )
      .catchSome {
        case ex : ResourceNotFoundException => ZIO.succeed(Response(status = Status.NotFound))
        case ex : NonWritableArchiveException => { println(ex); ZIO.succeed(Response(status = Status.InternalServerError)) }
        case ex : FailedConversionException => { println(ex); ZIO.succeed(Response(status = Status.BadRequest)) }
      }
}
