module OperationalWidgets.OTPsWidget
  ( otpsWidget
  )
  where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text, h1, p)
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (runExceptT)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor (void, (<$>))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Unit (Unit, unit)
import DataModel.OTPs (OTP(..), OTPUseInfo(..))
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff.Class (liftAff)
import Functions.User (getOTPs)
import Views.SimpleWebComponents (simpleButton)

data InternalAction = AddOTP | DeleteOTP OTP | LoadedOTPs (Either AppError (Array OTP))

otpsWidget :: WidgetState -> Widget HTML Unit
otpsWidget state = do
  res <- case state of
    Loading -> (otpsView Default []) <|> (LoadedOTPs <$> (liftAff $ runExceptT getOTPs))
    _ -> otpsView state []
  case res of
    AddOTP ->  pure unit
    DeleteOTP otp-> pure unit
    LoadedOTPs v -> pure unit
    
otpsView state otps = 
  case state of
    Error err -> div [] [
          h1 [] [text "One-Time Passwords"]
        , p [] [text err]
        , p [] [text "A one-time password works like your regular passphrase, but it can be used only once. Strongly recommended when accessing your Clipperz account from unsecure devices where keyloggers may be installed."]
        , simpleButton "Add" false AddOTP
        , div [] (toP <$> otps)
        ]
    _ -> div [] [
          h1 [] [text "One-Time Passwords"]
        , p [] [text "A one-time password works like your regular passphrase, but it can be used only once. Strongly recommended when accessing your Clipperz account from unsecure devices where keyloggers may be installed."]
        , simpleButton "Add" false AddOTP
        , div [] (toP <$> otps)
        ]

  where 
    toP :: OTP -> Widget HTML InternalAction
    toP v@(OTP { otp, timestamp, used }) = div [] ([ simpleButton "Delete" false (DeleteOTP v)
                                                  , p [] [text otp]
                                                  , p [] [text ("Created on " <> (show timestamp))]
                                                  ] <> (usedElem used))
    usedElem Nothing = []
    usedElem (Just (OTPUseInfo { dateTime, reason })) = [p [] [text (((show reason) <> " on " <> (show dateTime)))]]                                                                               
