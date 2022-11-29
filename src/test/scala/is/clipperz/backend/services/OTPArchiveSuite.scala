package is.clipperz.backend.services

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{ Files, Paths, FileSystems }
import java.security.MessageDigest
import scala.language.postfixOps
import zio.{ Chunk, ZIO }
import zio.stream.{ ZStream, ZSink }
import zio.test.Assertion.{ nothing, throws, throwsA, fails, isSubtype, anything }
import zio.test.{ ZIOSpecDefault, assertTrue, assert, assertCompletes, assertZIO, TestAspect }
import zio.json.EncoderOps
import zhttp.http.{ Version, Headers, Method, URL, Request, Body }
import zhttp.http.*
import is.clipperz.backend.Main
import java.nio.file.Path
import _root_.is.clipperz.backend.exceptions.ResourceNotFoundException
import is.clipperz.backend.functions.FileSystem
import is.clipperz.backend.exceptions.EmptyContentException
import zio.Clock
import zio.Clock.ClockLive
import zio.test.TestClock
import zio.Duration
import is.clipperz.backend.data.HexString
import is.clipperz.backend.exceptions.BadRequestException
import zio.test.TestEnvironment
import is.clipperz.backend.exceptions.NonAuthorizedException
import zio.ZLayer

object OTPArchiveSpec extends ZIOSpecDefault:
  val otpBasePath = FileSystems.getDefault().nn.getPath("target", "tests", "archive", "otps").nn

  val environment = OTPArchive.fs(otpBasePath, 2, false)

  val otpHash = HexString("d1d733a8041744d6e4b7b991b5f38df48a3767acd674c9df231c920323232320")
  val goodVerifier = HexString("d1d733a8041744d6e4b7b991b5f38df48a3767acd674c9df231c92068801a460")
  val badVerifier = HexString("d1d733a8041744d6e4b7b991b5")
  val encryptedPassword = HexString("d1d733a8041744d6e4b7b991b5f38df48a3767acd674c9df231c92068801a789")

  val goodBlob = OTPBlob(goodVerifier, encryptedPassword)
  val badBlob = OTPBlob(badVerifier, encryptedPassword)
  

  def spec = suite("BlobArchive")(
    test("getOTP - fail - not found") {
      for {
        archive <- ZIO.service[OTPArchive]
        res <- assertZIO(archive.getOTPBlob(otpHash, goodVerifier).exit)(fails(isSubtype[ResourceNotFoundException](anything)))
      } yield res
    } @@ TestAspect.before(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn))) 
      @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn))),
    test("deleteOTP - fail - not found") {
      for {
        archive <- ZIO.service[OTPArchive]
        res <- assertZIO(archive.deleteOTPBlob(otpHash, goodBlob).exit)(fails(isSubtype[ResourceNotFoundException](anything)))
      } yield res
    } @@ TestAspect.before(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn)))
      @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn))),
    test("saveOTP - success") {
      for {
          archive <- ZIO.service[OTPArchive]
          key <- archive.saveOTPBlob(otpHash, goodBlob)
          content <- archive.getOTPBlob(otpHash, goodVerifier)
        } yield assertTrue(key == otpHash, content == goodBlob)
    } @@ TestAspect.before(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn)))
      @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn))),
    test("getOTP - success") {
      for {
          archive <- ZIO.service[OTPArchive]
          _ <- archive.saveOTPBlob(otpHash, goodBlob)
          content <- archive.getOTPBlob(otpHash, goodVerifier)
        } yield assertTrue(content == goodBlob)
    } @@ TestAspect.before(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn)))
      @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn))),
    test("getOTP - fail - wrong verifier") {
      for {
          archive <- ZIO.service[OTPArchive]
          _ <- archive.saveOTPBlob(otpHash, goodBlob)
          res <- assertZIO(archive.getOTPBlob(otpHash, badVerifier).exit)(fails(isSubtype[NonAuthorizedException](anything)))
        } yield res
    } @@ TestAspect.before(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn)))
      @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn))),
    test("getOTP after wrong verifier - fail - not found") {
      for {
          archive <- ZIO.service[OTPArchive]
          _ <- archive.saveOTPBlob(otpHash, goodBlob)
          _ <- assertZIO(archive.getOTPBlob(otpHash, badVerifier).exit)(fails(isSubtype[NonAuthorizedException](anything)))
          res <- assertZIO(archive.getOTPBlob(otpHash, goodVerifier).exit)(fails(isSubtype[ResourceNotFoundException](anything)))
        } yield res
    } @@ TestAspect.before(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn)))
      @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn))),
    test("deleteOTP - fail - different blob") {
      for {
          archive <- ZIO.service[OTPArchive]
          _ <- archive.saveOTPBlob(otpHash, goodBlob)
          res <- assertZIO(archive.deleteOTPBlob(otpHash, badBlob).exit)(fails(isSubtype[NonAuthorizedException](anything)))
        } yield res
    } @@ TestAspect.before(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn)))
      @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn))),
    test("deleteOTP - success") {
      for {
          archive <- ZIO.service[OTPArchive]
          _ <- archive.saveOTPBlob(otpHash, goodBlob)
          _ <- archive.deleteOTPBlob(otpHash, goodBlob)
          res <- assertZIO(archive.getOTPBlob(otpHash, goodVerifier).exit)(fails(isSubtype[ResourceNotFoundException](anything)))
        } yield res
    } @@ TestAspect.before(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn)))
      @@ TestAspect.after(ZIO.succeed(FileSystem.deleteAllFiles(otpBasePath.toFile().nn)))
  ).provideSomeLayerShared(environment) @@
    TestAspect.sequential
