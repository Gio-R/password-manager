module Functions.Login where

import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (ExceptT(..), withExceptT)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (fromArrayBuffer)
import Data.Maybe (Maybe(..))
import Data.PrettyShow (prettyShow)
import Data.Unit (Unit)
import DataModel.Credentials (Credentials)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Functions.Communication.Login (login)
import Functions.JSState (updateAppState)
import Functions.State (getSRPConf)
import Functions.SRP as SRP

doLogin :: Credentials -> ExceptT String Aff Unit
doLogin { username, password } = do
  conf <- withExceptT (prettyShow) (ExceptT $ liftEffect getSRPConf)
  c    <- ExceptT $ Right <$> fromArrayBuffer <$> SRP.prepareC conf username password
  p    <- ExceptT $ Right <$> fromArrayBuffer <$> SRP.prepareP conf username password

  withExceptT (prettyShow) (ExceptT $ updateAppState { username: Just username, c: Just c, p: Just p })

  indexReference <- withExceptT (\_ -> "Login failed") login
  pure $ indexReference
