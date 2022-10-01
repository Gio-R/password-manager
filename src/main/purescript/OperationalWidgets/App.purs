module OperationalWidgets.App where

import Concur.Core (Widget)
import Concur.React (HTML)
import Control.Bind (bind, discard)
import Data.Function (($), flip)
import Data.Functor (void)
import Data.Unit (Unit)
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.SRP as SRP
import Functions.State (computeInitialState)
import Functions.JSState (updateAppState)
import OperationalWidgets.HomePageWidget as HomePageWidget
import Views.LoginFormView (emptyForm)
import Views.LandingPageView (landingPageView, LandingPageView(..))

import Data.Show (show)
import Data.Semigroup ((<>))
import Effect.Class.Console (log)

app :: Widget HTML Unit
app = do
  initialState <- liftEffect computeInitialState
  liftAff $ updateAppState initialState
  _ <- do
    indexReference <- landingPageView SRP.baseConfiguration (LoginView Default emptyForm)
    void $ HomePageWidget.homePageWidget indexReference
  app