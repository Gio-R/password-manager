module DataModel.User where

import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Bifunctor (rmap)
import Data.Eq (class Eq)
import Data.HexString (HexString)
import Data.Maybe (Maybe)
import DataModel.Password (PasswordGeneratorSettings)

newtype UserCard =
  UserCard
    { c :: HexString
    , v :: HexString
    , s :: HexString
    , srpVersion :: String
    , masterKeyEncodingVersion :: String
    , masterKeyContent :: HexString
    }

instance encodeJsonUserCard :: EncodeJson UserCard where
  encodeJson (UserCard record) = encodeJson record

instance decodeJsonUserCard :: DecodeJson UserCard where
  decodeJson json = rmap (\record -> UserCard record) (decodeJson json)

newtype IndexReference =
  IndexReference
    { reference :: HexString
    , masterKey :: HexString
    , indexVersion :: String
    }

instance encodeJsonIndexReference :: EncodeJson IndexReference where
  encodeJson (IndexReference record) = encodeJson record

instance decodeJsonIndexReference :: DecodeJson IndexReference where
  decodeJson json = rmap (\record -> IndexReference record) (decodeJson json)

newtype UserPreferences = 
  UserPreferences
    { passwordGeneratorSettings :: PasswordGeneratorSettings
    , automaticLock :: Maybe Int
    }

instance encodeJsonUserPreferences :: EncodeJson UserPreferences where
  encodeJson (UserPreferences record) = encodeJson record

instance decodeJsonUserPreferences :: DecodeJson UserPreferences where
  decodeJson json = rmap (\record -> UserPreferences record) (decodeJson json)

derive instance eqUserPreferences :: Eq UserPreferences

newtype UserPreferencesReference =
  UserPreferencesReference
    { reference :: HexString
    , key :: HexString
    }

instance encodeJsonUserPreferencesReference :: EncodeJson UserPreferencesReference where
  encodeJson (UserPreferencesReference record) = encodeJson record

instance decodeJsonUserPreferencesReference :: DecodeJson UserPreferencesReference where
  decodeJson json = rmap (\record -> UserPreferencesReference record) (decodeJson json)

newtype OTPReference =
  OTPReference
    { otp :: String
    }

instance encodeJsonOTPReference :: EncodeJson OTPReference where
  encodeJson (OTPReference record) = encodeJson record

instance decodeJsonOTPReference :: DecodeJson OTPReference where
  decodeJson json = rmap (\record -> OTPReference record) (decodeJson json)

newtype UserInfoReferences =
  UserInfoReferences 
    { preferencesReference :: UserPreferencesReference
    , indexReference :: IndexReference
    , otpReferences :: Array OTPReference
    }

instance encodeJsonUserInfoReferences :: EncodeJson UserInfoReferences where
  encodeJson (UserInfoReferences record) = encodeJson record

instance decodeJsonUserInfoReferences :: DecodeJson UserInfoReferences where
  decodeJson json = rmap (\record -> UserInfoReferences record) (decodeJson json)
