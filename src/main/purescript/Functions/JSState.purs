module Functions.JSState where

import Control.Bind (bind)
import Control.Applicative (pure)
import Control.Semigroupoid ((>>>))
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>), void)
import Data.Show (show)
import Data.Unit (Unit, unit)
import DataModel.AppState (AppState, AppError(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Record (merge)

foreign import getJsonState :: Unit -> Effect String

getAppState :: Effect (Either AppError AppState)
getAppState = do
  json <- jsonParser <$> (getJsonState unit)
  let appstate = decodeJson <$> json 
  case appstate of
    Left s -> pure $ Left $ InvalidStateError s
    Right (Left s) -> pure $ Left $ InvalidStateError $ show s
    Right (Right a) -> pure $ Right a

foreign import updateJsonState :: String -> Effect Unit

modifyAppState :: AppState -> Aff Unit
modifyAppState = encodeJson >>> stringify >>> updateJsonState >>> liftEffect

updateAppState partialState = runExceptT $ do
  stateToUpdate <- ExceptT $ liftEffect $ getAppState
  ExceptT $ Right <$> (void $ modifyAppState (merge partialState stateToUpdate))
