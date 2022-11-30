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

  // val saveOTPWithSession = Request(
  //   url = URL(!! / "otps"),
  //   method = Method.POST,
  //   headers = Headers((SessionManager.sessionKeyHeaderName, sessionKey)),
  //   version = Version.Http_1_1,
  // )

  // val saveOTPWithoutSession = Request(
  //   url = URL(!! / "logout"),
  //   method = Method.POST,
  //   headers = Headers.empty,
  //   version = Version.Http_1_1,
  // )

  def spec = suite("LogoutApis")(
    test("DELETE not found -> 404") {
      assertNever("Not yet implemented")
    } @@ TestAspect.before(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn)))
      @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn))),
    test("DELETE no session -> 400") {
      assertNever("Not yet implemented")
    } @@ TestAspect.before(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn)))
      @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn))), 
    test("GET not found -> 404") {
      assertNever("Not yet implemented")
    } @@ TestAspect.before(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn)))
      @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn))),
    test("GET no session -> 400") {
      assertNever("Not yet implemented")
    } @@ TestAspect.before(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn)))
      @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn))),
    test("POST no session -> 400") {
      assertNever("Not yet implemented")
    } @@ TestAspect.before(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn)))
      @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn))),
    test("POST -> 200") {
      assertNever("Not yet implemented")
    } @@ TestAspect.before(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn)))
      @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn))),
    test("POST / GET -> 200, 200, content") {
      assertNever("Not yet implemented")
    } @@ TestAspect.before(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn)))
      @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn))),
    test("POST / GET / GET -> 200, 200, 404") {
      assertNever("Not yet implemented")
    } @@ TestAspect.before(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn)))
      @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn))),
    test("POST / GET bad verifier -> 200, 402") {
      assertNever("Not yet implemented")
    } @@ TestAspect.before(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn)))
      @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn))),
    test("POST / GET bad verifier / GET -> 200, 402, 404") {
      assertNever("Not yet implemented")
    } @@ TestAspect.before(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn)))
      @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn))),
    test("POST / DELETE / GET -> 200, 200, 404") {
      assertNever("Not yet implemented")
    } @@ TestAspect.before(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn)))
      @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn)))
  ).provideLayerShared(environment) @@
    TestAspect.sequential
