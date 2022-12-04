package is.clipperz.backend.apis

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Paths, FileSystems }
import java.security.MessageDigest
import scala.language.postfixOps
import zio.{ Chunk, ZIO, Task }
import zio.stream.{ ZStream, ZSink }
import zio.test.Assertion.{ nothing, isTrue }
import zio.test.{ ZIOSpecDefault, assertZIO, assertNever, assertTrue, assert, TestAspect }
import zio.json.EncoderOps
import zhttp.http.{ Version, Headers, Method, URL, Request, Body }
import zhttp.http.*
import is.clipperz.backend.Main
import is.clipperz.backend.data.HexString
import is.clipperz.backend.data.HexString.bytesToHex
import is.clipperz.backend.functions.crypto.HashFunction
import java.nio.file.Path
import is.clipperz.backend.functions.FileSystem
import is.clipperz.backend.services.SaveBlobData
import is.clipperz.backend.services.PRNG
import is.clipperz.backend.services.SessionManager
import is.clipperz.backend.services.UserArchive
import is.clipperz.backend.services.BlobArchive
import is.clipperz.backend.services.TollManager
import is.clipperz.backend.services.SrpManager
import is.clipperz.backend.functions.Conversions.{ bytesToBigInt, bigIntToBytes }
import is.clipperz.backend.functions.fromStream
import is.clipperz.backend.services.UserCard
import java.sql.Blob
import zio.test.ZIOSpec
import zio.Scope
import zio.ZLayer
import is.clipperz.backend.services.SRPStep1Data
import is.clipperz.backend.data.srp.RFCTestVector
import is.clipperz.backend.services.SRPStep1Response
import is.clipperz.backend.services.SRPStep2Data
import is.clipperz.backend.functions.SrpFunctions.SrpFunctionsV6a
import is.clipperz.backend.functions.SrpFunctions
import is.clipperz.backend.services.SRPStep2Response
import is.clipperz.backend.services.OTPArchive
import is.clipperz.backend.services.OTPBlob
import is.clipperz.backend.services.SaveOTPBlobData
import is.clipperz.backend.services.UsedOTPBlob
import is.clipperz.backend.services.SaveUsedOTPBlobData
import java.time.Instant
import is.clipperz.backend.services.OTPUseCases
import is.clipperz.backend.services.Session

object OTPSpec extends ZIOSpecDefault:
  val app = Main.clipperzBackend
  val blobBasePath = FileSystems.getDefault().nn.getPath("target", "tests", "archive", "blobs").nn
  val userBasePath = FileSystems.getDefault().nn.getPath("target", "tests", "archive", "users").nn
  val otpBasePath = FileSystems.getDefault().nn.getPath("target", "tests", "archive", "otps").nn

  val environment =
    PRNG.live ++
      SessionManager.live ++
      UserArchive.fs(userBasePath, 2, false) ++
      BlobArchive.fs(blobBasePath, 2, false) ++
      OTPArchive.fs(otpBasePath, 2, false) ++
      ((UserArchive.fs(userBasePath, 2, false) ++ PRNG.live) >>> SrpManager.v6a()) ++
      (PRNG.live >>> TollManager.live)

  val sessionKey = "sessionKey"

  def prepareDelete(key: HexString, blob: OTPBlob, session: Boolean): Request =
    Request(
      url = URL(!! / "otps" / key.toString),
      method = Method.DELETE,
      headers = if (session) Headers((SessionManager.sessionKeyHeaderName, sessionKey)) else Headers.empty,
      body = Body.fromString(SaveOTPBlobData(key, blob).toJson, StandardCharsets.UTF_8.nn),
      version = Version.Http_1_1
    )

  def prepareDelete(key: HexString, blob: UsedOTPBlob, session: Boolean): Request =
    Request(
      url = URL(!! / "otps" / key.toString),
      method = Method.DELETE,
      headers = if (session) Headers((SessionManager.sessionKeyHeaderName, sessionKey)) else Headers.empty,
      body = Body.fromString(SaveUsedOTPBlobData(key, blob).toJson, StandardCharsets.UTF_8.nn),
      version = Version.Http_1_1
    )

  def prepareSave(key: HexString, blob: OTPBlob, session: Boolean): Request =
    Request(
      url = URL(!! / "otps" / key.toString),
      method = Method.POST,
      headers = if (session) Headers((SessionManager.sessionKeyHeaderName, sessionKey)) else Headers.empty,
      body = Body.fromString(SaveOTPBlobData(key, blob).toJson, StandardCharsets.UTF_8.nn),
      version = Version.Http_1_1
    )

  def prepareGet(key: HexString, verifier: HexString, session: Boolean): Request =
    Request(
      url = URL(!! / "otps" / s"${key.toString}", queryParams = Map(("verifier", List(verifier.toString())))),
      method = Method.GET,
      headers = if (session) Headers((SessionManager.sessionKeyHeaderName, sessionKey)) else Headers.empty,
      version = Version.Http_1_1
    )
  
  val otpHash = HexString("d1d733a8041744d6e4b7b991b5f38df48a3767acd674c9df231c920323232320")
  val goodVerifier = HexString("d1d733a8041744d6e4b7b991b5f38df48a3767acd674c9df231c92068801a460")
  val badVerifier = HexString("d1d733a8041744d6e4b7b991b5")
  val encryptedPassword = HexString("d1d733a8041744d6e4b7b991b5f38df48a3767acd674c9df231c92068801a789")

  val goodBlob = OTPBlob(goodVerifier, encryptedPassword)
  val badBlob = OTPBlob(badVerifier, encryptedPassword)
  
  val badUsedOTPBlob = UsedOTPBlob(badVerifier, Instant.now().nn, OTPUseCases.USED)

  def prepareSession(c: String): ZIO[SessionManager, Throwable, Unit] =
    ZIO
      .service[SessionManager]
      .flatMap(sessionManager => sessionManager.saveSession(Session(sessionKey, Map(("c", c)))))
      .map(_ => ())

  def deleteSession(): ZIO[SessionManager, Throwable, Unit] =
    ZIO
      .service[SessionManager]
      .flatMap(sessionManager => sessionManager.deleteSession(sessionKey))
      .map(_ => ())

  val c = "7815018e9d84b5b0f319c87dee46c8876e85806823500e03e72c5d66e5d40456"

  def spec = suite("LogoutApis")(
    test("DELETE not found -> 404") {
      for {
        statusCodeDelete <- app(prepareDelete(otpHash, goodBlob, true)).map(response => response.status.code)
      } yield assertTrue(statusCodeDelete == 404)
    } @@ TestAspect.before(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn)))
      @@ TestAspect.before(prepareSession(c))
      @@ TestAspect.after(deleteSession())
      @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn))),
    // test("DELETE no session -> 400") {
    //   for {
    //     statusCodeDelete <- app(prepareDelete(otpHash, goodBlob, false)).map(response => response.status.code)
    //   } yield assertTrue(statusCodeDelete == 400)
    // } @@ TestAspect.before(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn)))
    //   @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn))), 
    test("GET not found -> 404") {
      for {
        statusCodeGet <- app(prepareGet(otpHash, goodVerifier, true)).map(response => response.status.code)
      } yield assertTrue(statusCodeGet == 404)
    } @@ TestAspect.before(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn)))
      @@ TestAspect.before(prepareSession(c))
      @@ TestAspect.after(deleteSession())
      @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn))),
    // test("GET no session -> 400") {
    //   for {
    //     statusCodeGet <- app(prepareGet(otpHash, goodVerifier, false)).map(response => response.status.code)
    //   } yield assertTrue(statusCodeGet == 400)
    // } @@ TestAspect.before(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn)))
    //   @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn))),
    // test("POST no session -> 400") {
    //   for {
    //     statusCodePost <- app(prepareSave(otpHash, goodBlob, false)).map(response => response.status.code)
    //   } yield assertTrue(statusCodePost == 400)
    // } @@ TestAspect.before(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn)))
    //   @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn))),
    test("POST -> 200") {
      for {
        statusCodePost <- app(prepareSave(otpHash, goodBlob, true)).map(response => response.status.code)
      } yield assertTrue(statusCodePost == 200)
    } @@ TestAspect.before(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn)))
      @@ TestAspect.before(prepareSession(c))
      @@ TestAspect.after(deleteSession())
      @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn))),
    test("POST / GET -> 200, 200, content") {
      for {
        statusCodePost <- app(prepareSave(otpHash, goodBlob, true)).map(response => response.status.code)
        response <- app(prepareGet(otpHash, goodVerifier, true))
        blob <- fromStream[OTPBlob](response.body.asStream)
      } yield assertTrue(statusCodePost == 200, response.status.code == 200, blob == goodBlob)
    } @@ TestAspect.before(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn)))
      @@ TestAspect.before(prepareSession(c))
      @@ TestAspect.after(deleteSession())
      @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn))),
    test("POST / GET / GET -> 200, 200, 200, content") {
      for {
        statusCodePost <- app(prepareSave(otpHash, goodBlob, true)).map(response => response.status.code)
        statusCodeGet <- app(prepareGet(otpHash, goodVerifier, true)).map(response => response.status.code)
        response <- app(prepareGet(otpHash, goodVerifier, true))
        blob <- fromStream[UsedOTPBlob](response.body.asStream)
      } yield assertTrue(statusCodePost == 200, statusCodeGet == 200, response.status.code == 200, blob.verifier == goodVerifier, blob.useCase == OTPUseCases.USED)
    } @@ TestAspect.before(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn)))
      @@ TestAspect.before(prepareSession(c))
      @@ TestAspect.after(deleteSession())
      @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn))),
    test("POST / GET bad verifier -> 200, 403") {
      for {
        statusCodePost <- app(prepareSave(otpHash, goodBlob, true)).map(response => response.status.code)
        statusCodeGet <- app(prepareGet(otpHash, badVerifier, true)).map(res => res.status.code)
      } yield assertTrue(statusCodePost == 200, statusCodeGet == 403)
    } @@ TestAspect.before(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn)))
      @@ TestAspect.before(prepareSession(c))
      @@ TestAspect.after(deleteSession())
      @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn))),
    test("POST / GET bad verifier / GET -> 200, 403, 200, content") {
      for {
        statusCodePost <- app(prepareSave(otpHash, goodBlob, true)).map(response => response.status.code)
        statusCodeGet <- app(prepareGet(otpHash, badVerifier, true)).map(res => res.status.code)
        response <- app(prepareGet(otpHash, goodVerifier, true))
        blob <- fromStream[UsedOTPBlob](response.body.asStream)
      } yield assertTrue(statusCodePost == 200, statusCodeGet == 403, response.status.code == 200, blob.verifier == goodVerifier, blob.useCase == OTPUseCases.DISABLED)
    } @@ TestAspect.before(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn)))
      @@ TestAspect.before(prepareSession(c))
      @@ TestAspect.after(deleteSession())
      @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn))),
    test("POST / DELETE / GET -> 200, 200, 404") {
      for {
        statusCodePost <- app(prepareSave(otpHash, goodBlob, true)).map(response => response.status.code)
        statusCodeDelete <- app(prepareDelete(otpHash, goodBlob, true)).map(response => response.status.code)
        statusCodeGet <- app(prepareGet(otpHash, goodVerifier, true)).map(response => response.status.code)
      } yield assertTrue(statusCodePost == 200, statusCodeDelete == 200, statusCodeGet == 404)
    } @@ TestAspect.before(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn)))
      @@ TestAspect.before(prepareSession(c))
      @@ TestAspect.after(deleteSession())
      @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn)))
  ).provideLayerShared(environment) @@
    TestAspect.sequential
