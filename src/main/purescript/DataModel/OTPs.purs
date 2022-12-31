module DataModel.OTPs where

import Control.Semigroupoid ((<<<))
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Encoders (encodeString)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Decoders (decodeString)
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Bifunctor (rmap)
import Data.Time.Duration (Milliseconds(..))
import Data.DateTime (DateTime)
import Data.DateTime.Instant (fromDateTime, toDateTime, unInstant, instant)
import Data.Either (Either(..))
import Data.Function (($))
import Data.HexString (HexString)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Show (class Show, show)

data OTPUseCases = Used | Disabled
instance showOTPUseCases :: Show OTPUseCases where
  show Used = "USED"
  show Disabled = "DISABLED"

instance encodeJsonOTPUseCases :: EncodeJson OTPUseCases where
  encodeJson u = encodeString $ show u

instance decodeJsonOTPUseCases :: DecodeJson OTPUseCases where
  decodeJson json = 
    let decodedValue = decodeString json
    in case decodedValue of
      Left err -> Left err
      Right "USED" -> Right Used
      Right "DISABLED" -> Right Disabled
      Right _ -> Left $ UnexpectedValue json

newtype OTPUseInfo = OTPUseInfo { dateTime :: DateTime, reason :: OTPUseCases } 
instance encodeJsonOTPUseInfo :: EncodeJson OTPUseInfo where
  encodeJson (OTPUseInfo record) = encodeJson { reason: record.reason, dateTime: fromDateTimeToNumber record.dateTime }

instance decodeJsonOTPUseInfo :: DecodeJson OTPUseInfo where
  decodeJson json = 
    case (decodeJson json :: Either JsonDecodeError { reason :: OTPUseCases, dateTime :: Number }) of
      Left err -> Left err
      Right { reason, dateTime } ->
        case instant (Milliseconds dateTime) of
          Nothing -> Left $ UnexpectedValue json
          Just instant -> Right $ OTPUseInfo { reason, dateTime: toDateTime instant }

newtype OTP = OTP {
  otp :: String
, timestamp :: DateTime
, used :: Maybe OTPUseInfo
}

instance encodeJsonOTP :: EncodeJson OTP where
  encodeJson (OTP record) = encodeJson { otp: record.otp, timestamp: fromDateTimeToNumber record.timestamp, used: record.used }

instance decodeJsonOTP :: DecodeJson OTP where
  decodeJson json = 
    case (decodeJson json :: Either JsonDecodeError { otp :: String, timestamp :: Number, used :: Maybe OTPUseInfo }) of
      Left err -> Left err
      Right { otp, timestamp, used } ->
        case instant (Milliseconds timestamp) of
          Nothing -> Left $ UnexpectedValue json
          Just instant -> Right $ OTP { otp, timestamp: toDateTime instant, used }

newtype OTPBlob = OTPBlob {
  verifier :: HexString
, encryptedPassword :: HexString
}

instance encodeJsonOTPBlob :: EncodeJson OTPBlob where
  encodeJson (OTPBlob record) = encodeJson record

instance decodeJsonOTPBlob :: DecodeJson OTPBlob where
  decodeJson json = rmap (\record -> OTPBlob record) (decodeJson json)

newtype UsedOTPBlob = UsedOTPBlob { 
  verifier :: HexString
, dateTime :: DateTime
, useCase  :: OTPUseCases
}

instance encodeJsonUsedOTPBlob :: EncodeJson UsedOTPBlob where
  encodeJson (UsedOTPBlob record) = encodeJson { verifier: record.verifier
                                               , dateTime: fromDateTimeToNumber record.dateTime
                                               , useCase: record.useCase
                                               }

instance decodeJsonUsedOTPBlob :: DecodeJson UsedOTPBlob where
  decodeJson json = 
    case (decodeJson json :: Either JsonDecodeError { verifier :: HexString, dateTime :: Number, useCase :: OTPUseCases}) of
      Left err -> Left err
      Right { verifier, dateTime, useCase } ->
        case instant (Milliseconds dateTime) of
          Nothing -> Left $ UnexpectedValue json
          Just instant -> Right $ UsedOTPBlob { verifier, useCase, dateTime: toDateTime instant }

fromDateTimeToNumber :: DateTime -> Number
fromDateTimeToNumber = unwrap <<< unInstant <<< fromDateTime
