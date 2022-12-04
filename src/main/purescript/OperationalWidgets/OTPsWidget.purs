module OperationalWidgets.OTPsWidget
  ( otpsWidget
  )
  where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text, h1, p)
import Data.Unit (Unit)
import DataModel.WidgetState (WidgetState(..))
import Views.SimpleWebComponents (simpleButton)

data InternalAction = AddOTP | DeleteOTP | ListOTPs

otpsWidget :: WidgetState -> Widget HTML Unit
otpsWidget state = do
  res <- case state of
    Default -> otpsView []
    Loading -> (otpsView []) -- <|> (ListOTPs <$> getOTPs)
    Error err -> div [] [text err, otpsView []]
  case res of
    AddOTP ->
    DeleteOTP ->
    ListOTPs ->
    
otpsView state otps = 
  case state of
    Error err -> div [] [
          h1 [] [text "One-Time Passwords"]
        , p [] [text err]
        , p [] [text "A one-time password works like your regular passphrase, but it can be used only once. Strongly recommended when accessing your Clipperz account from unsecure devices where keyloggers may be installed."]
        , simpleButton "Add" false AddOTP
        -- , otps
        ]
    _ -> div [] [
          h1 [] [text "One-Time Passwords"]
        , p [] [text "A one-time password works like your regular passphrase, but it can be used only once. Strongly recommended when accessing your Clipperz account from unsecure devices where keyloggers may be installed."]
        , simpleButton "Add" false AddOTP
        -- , otps
        ]
