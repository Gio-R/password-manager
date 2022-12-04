package is.clipperz.backend.apis

import java.io.FileNotFoundException
import zio.ZIO
import zio.stream.ZStream
import zio.json.EncoderOps
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
import is.clipperz.backend.services.SessionManager
import is.clipperz.backend.services.SaveUsedOTPBlobData
import is.clipperz.backend.exceptions.NonAuthorizedException

val otpsApi: ClipperzHttpApp = Http.collectZIO {
  case request @ Method.POST -> !! / "otps" / hash =>
    ZIO
      .service[OTPArchive]
      .zip(ZIO.succeed(request.body.asStream))
      .flatMap((archive, bytes) =>
        fromStream[SaveOTPBlobData](bytes)
          .flatMap(saveData => archive.saveOTPBlob(saveData.key, saveData.otpBlob))
      )
      .map(results => Response.text(s"${results}"))
      .catchSome {
        case ex: EmptyContentException =>
          ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
        case ex: BadRequestException =>
          ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
        case ex: NonWritableArchiveException =>
          ZIO.logFatalCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.InternalServerError))
        case ex: FailedConversionException =>
          ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
        case ex => ZIO.logFatalCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.InternalServerError))
      } @@ LogAspect.logAnnotateRequestData(request)

  case request @ Method.DELETE -> !! / "otps" / hash =>
    ZIO
      .service[OTPArchive]
      // .zip(ZIO.service[SessionManager])
      .zip(ZIO.succeed(request.body.asStream))
      .flatMap((otpArchive, /* sessionManager, */ content) =>
        fromStream[SaveOTPBlobData](content)
          .flatMap(otpBlob =>
            if otpBlob.key == HexString(hash) then
              otpArchive.deleteOTPBlob(otpBlob.key, Right(otpBlob.otpBlob))
            else
              ZIO.fail(new BadRequestException("Hash and key do not correspond"))
          )
          .catchSome {
            case ex: FailedConversionException =>
              fromStream[SaveUsedOTPBlobData](content)
                .flatMap(usedBlob =>
                  if usedBlob.key == HexString(hash) then
                    otpArchive.deleteOTPBlob(usedBlob.key, Left(usedBlob.otpBlob))
                  else
                    ZIO.fail(new BadRequestException("Hash and key do not correspond"))
                )
          }
          .map(b => if b then Response.ok else Response(status = Status.NotFound))
      )
      .catchSome {
        case ex: NonAuthorizedException => ZIO.succeed(Response(status = Status.Forbidden))
        case ex: ResourceNotFoundException => ZIO.succeed(Response(status = Status.NotFound))
        case ex: BadRequestException =>
          ZIO.logInfoCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
        case ex: NonWritableArchiveException =>
          ZIO.logFatalCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.InternalServerError))
        case ex: FailedConversionException =>
          ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
        case ex => ZIO.logFatalCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.InternalServerError))
      } @@ LogAspect.logAnnotateRequestData(request) 

  case request @ Method.GET -> !! / "otps" / hash =>
    ZIO
      .service[OTPArchive]
      // .zip(ZIO.service[SessionManager])
      .zip(ZIO.succeed(request.body.asStream))
      .flatMap((otpArchive, /* sessionManager, */ content) =>
        val verifiers = request.url.queryParams.get("verifier")
        verifiers match
          case None => ZIO.fail(new BadRequestException("No verifier sent"))
          case Some(ls) =>
            if ls.length == 1 then
              otpArchive
                .getOTPBlob(HexString(hash), HexString(ls(0)))
                .map(b =>
                  b match
                    case Left(o) => Response.json(o.toJson)
                    case Right(o) => Response.json(o.toJson)
                )
            else 
              ZIO.fail(new BadRequestException("No verifier sent"))
      )
      .catchSome {
        case ex: NonAuthorizedException => ZIO.succeed(Response(status = Status.Forbidden))
        case ex: ResourceNotFoundException => ZIO.succeed(Response(status = Status.NotFound))
        case ex: BadRequestException =>
          ZIO.logInfoCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
        case ex: NonWritableArchiveException =>
          ZIO.logFatalCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.InternalServerError))
        case ex: FailedConversionException =>
          ZIO.logWarningCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.BadRequest))
        case ex => ZIO.logFatalCause(s"${ex.getMessage()}", Cause.fail(ex)).as(Response(status = Status.InternalServerError))
      } @@ LogAspect.logAnnotateRequestData(request) 

}
