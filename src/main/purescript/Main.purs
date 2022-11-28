module Main
  ( main
  , registration
  , share
  , testLogin
  )
  where

import Data.Maybe (Maybe(..))
import Data.Unit (Unit)
import Concur.React.Run (runWidgetInDom)
import Effect (Effect)
import OperationalWidgets.App (app, Page(..), SharedCardReference, doTestLogin)

import Control.Bind (bind, discard)
import Effect.Class.Console (log)

main :: Effect Unit
main = runWidgetInDom "app" (app (Loading (Just Login)))

registration :: Effect Unit
registration = runWidgetInDom "app" (app Signup)

share :: String -> Effect Unit
share token = runWidgetInDom "app" (app (Share (Just token)))

-- testLogin :: String -> String -> Effect Unit
testLogin = do
  runWidgetInDom "app" (doTestLogin "joe" "clipperz")
