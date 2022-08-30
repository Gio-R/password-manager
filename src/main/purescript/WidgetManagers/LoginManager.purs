module WidgetManagers.LoginManager where

import Concur.Core (Widget)
import Concur.Core.FRP (demandLoop, loopW)
import Concur.React (HTML)
import Concur.React.DOM (div, text)
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.State (StateT)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT, withExceptT)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (HexString, fromArrayBuffer)
import DataModel.AppState (AppState)
import Effect.Aff.Class (liftAff)
import Record (merge)
import RestBackendCommunication
import SRP as SRP
import Utilities (makeStateT)
import Widgets.LoginForm (loginForm)

type LoginManagerResult = { c :: HexString, p :: HexString, indexReference :: HexString, sessionKey :: HexString }

loginManager :: SRP.SRPConf -> StateT AppState (Widget HTML) LoginManagerResult
loginManager conf = makeStateT $ demandLoop "" (\s -> loopW (Left s) (\err -> do
  loginFormResult@{ username, password } <- case err of
    Left string -> div [] [text $ string, loginForm]
    Right _     -> loginForm
  liftAff $ runExceptT $ do
    loginResult <- withExceptT (\_ -> "Login failed") (ExceptT $ login conf loginFormResult)
    c           <- ExceptT $ Right <$> SRP.prepareC conf username password
    p           <- ExceptT $ Right <$> SRP.prepareP conf username password
    pure $ merge {c: fromArrayBuffer c, p: fromArrayBuffer p} loginResult
))
