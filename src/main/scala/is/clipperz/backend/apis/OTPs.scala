package is.clipperz.backend.apis

import java.io.FileNotFoundException
import zio.ZIO
import zio.stream.ZStream
import zhttp.http.{ Headers, HeaderNames, HeaderValues, Http, Body, Method, Path, PathSyntax, Response, Status }
import zhttp.http.* //TODO: fix How do you import `!!` and `/`?
import is.clipperz.backend.data.HexString
import is.clipperz.backend.exceptions.{
  NonWritableArchiveException,
  NonReadableArchiveException,
  FailedConversionException,
  ResourceNotFoundException,
}
import is.clipperz.backend.functions.fromStream
import is.clipperz.backend.services.{ BlobArchive, SaveBlobData }
import is.clipperz.backend.Main.ClipperzHttpApp
import is.clipperz.backend.exceptions.EmptyContentException
import is.clipperz.backend.exceptions.BadRequestException
import zio.Cause
import is.clipperz.backend.LogAspect
import is.clipperz.backend.services.OTPArchive
import is.clipperz.backend.services.SaveOTPBlobData

val otpsApi: ClipperzHttpApp = Http.collectZIO {
  case request @ Method.POST -> !! / "otps" =>
    ZIO
      .service[OTPArchive]
      .zip(ZIO.succeed(request.body.asStream))
      .flatMap((archive, bytes) =>
        fromStream[SaveOTPBlobData](bytes)
          .flatMap(saveData => archive.saveOTPBlob(saveData.key, saveData.otpBlob))
      )
      .map(results => Response.text(s"${results}"))
      .catchSome {
        // case ex: EmptyContentException =>
        //   ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
        // case ex: BadRequestException =>
        //   ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
        // case ex: NonWritableArchiveException =>
        //   ZIO.logFatalCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.InternalServerError))
        // case ex: FailedConversionException =>
        //   ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
        case ex => ZIO.logFatalCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.InternalServerError))
      } @@ LogAspect.logAnnotateRequestData(request)

  case request @ Method.DELETE -> !! / "otps" / hash =>
    ZIO.succeed(Response(status = Status.NotImplemented)) @@ LogAspect.logAnnotateRequestData(request) 

  case request @ Method.GET -> !! / "otps" / hash =>
    ZIO.succeed(Response(status = Status.NotImplemented)) @@ LogAspect.logAnnotateRequestData(request)

}
