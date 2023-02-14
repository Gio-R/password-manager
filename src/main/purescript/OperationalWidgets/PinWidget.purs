module OperationalWidgets.PinWidget where

import Bytes (asArrayBuffer)
import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.Core.FRP (demand, fireOnce, loopW)
import Concur.React.DOM (h1, p, div, form, text, label, span, input, strong)
import Concur.React.Props as Props
import Control.Alternative ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Control.Monad.Except.Trans (ExceptT(..), runExceptT, except)
import Control.Semigroupoid ((<<<))
import Data.Array (elem)
import Data.Either (note, Either(..))
import Data.EuclideanRing ((/))
import Data.Function (($))
import Data.Functor ((<$>), void)
import Data.HexString (fromArrayBuffer, hex, toArrayBuffer)
import Data.HeytingAlgebra (not, (&&), (||))
import Data.Int (fromString)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Ord ((>), (>=), (<=))
import Data.Ring ((-))
import Data.Semigroup ((<>))
import Data.Semiring ((*))
import Data.Show (show)
import Data.String.CodeUnits (length, dropWhile)
import Data.String.Common (null)
import Data.Unfoldable (fromMaybe)
import Data.Unit (Unit)
import DataModel.AppState (AppError(..), InvalidStateError(..))
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.ArrayBuffer (concatArrayBuffers)
import Functions.EncodeDecode (encryptJson)
import Functions.JSState (getAppState)
import Functions.Pin (deleteCredentials, saveCredentials, generateKeyFromPin, makeKey, isPinValid)
import Functions.State (getHashFunctionFromAppState)
import Views.SimpleWebComponents (simpleButton, simpleInputWidget)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem, removeItem, Storage)
import Unsafe.Coerce (unsafeCoerce)

import Effect.Class.Console (log)

data PinWidgetAction = Reset | SetPin Int

setPinWidget :: forall a. WidgetState -> Widget HTML a
setPinWidget ws = do
  storage <- liftEffect $ window >>= localStorage
  maybeSavedUser <- liftEffect $ getItem (makeKey "user") storage
  let pinForm = formWidget (isJust maybeSavedUser)
  pinAction <- case ws of
    Default -> pinPage Nothing  pinForm
    Loading -> pinPage Nothing  pinForm
    Error e -> pinPage (Just e) pinForm
  eitherRes <- case pinAction of
    Reset -> (Right <$> (void $ pinPage Nothing (formWidget true))) <|> (liftEffect $ runExceptT (deleteCredentials storage))
    SetPin pin -> (Right <$> (void $ pinPage Nothing (formWidget true))) <|> (liftAff $ runExceptT (saveCredentials pin storage)) 
  case eitherRes of
    Left err -> setPinWidget (Error (show err))
    _        -> setPinWidget Default

  where 
    formWidget :: Boolean -> Widget HTML PinWidgetAction
    formWidget pinExists = div [Props.className "content"] [
      text $ "PIN is " <> (if pinExists then "" else "not ") <> "set on this device"
    , do
        signalResult <- demand $ do
          pin <- loopW "" (\value -> do
            result <- label [Props.className "pin"] [
              span [Props.className "label"] [text "PIN"]
            , input [
                Props._type "text"
              , Props.inputMode "numeric"
              , Props.placeholder (if pinExists then "*****" else "PIN")
              , Props.value value
              , Props.disabled pinExists
              , Props.pattern "^[0-9]+$"
              , (\e -> 
                  if (unsafeCoerce e).target.validity.valid
                  then Props.unsafeTargetValue e
                  else value
                ) <$> Props.onChange
              ]
            ]
            pure $  if (length result) > 5
                    then value
                    else result
          )
          result :: Maybe PinWidgetAction <- fireOnce (submitWidget pin pinExists)
          pure result
        pure signalResult
    ]

    submitWidget pin pinExists =
      if pinExists then
        simpleButton "reset" "Reset" false Reset
      else case fromString pin of
        Just p  -> simpleButton "save" "Save" (not (isPinValid p)) (SetPin p)
        Nothing -> simpleButton "save" "Save" true (SetPin 0)

pinPage :: forall a. Maybe String -> Widget HTML a -> Widget HTML a
pinPage error internalForm = div [Props._id "pinPage"] [ form [] [
  h1 [] [text "Device PIN"]
, div [Props.className "description"] $ [
    p [] [text "You may create a 5-digit PIN to be used instead of your passphrase. Please note that the PIN is specific to the device you are now using."]
  , p [] [strong [] [text "Warning"], text ": enabling a PIN on your device may represent a security risk! Make sure to keep the device with you at all times!"]  
  ] <> (fromMaybe $ (\t -> p [Props.className "error"] [text t]) <$> error)
, internalForm
]]
