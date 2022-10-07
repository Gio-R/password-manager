module OperationalWidgets.HomePageWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text)
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (runExceptT)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>), void)
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Data.Unit (Unit, unit)
import DataModel.AppState (AppError)
import DataModel.Index (CardReference, Index, IndexReference)
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff.Class (liftAff)
import Functions.Communication.Cards (getIndex)
import Functions.SRP as SRP
import Views.CardsManagerView (CardView(..))
import Views.SimpleWebComponents (simpleButton, loadingDiv)
import OperationalWidgets.CardsManagerWidget (cardsManagerWidget)

data HomePageAction = Loaded (Either AppError Index) | LogoutAction

homePageWidget :: SRP.SRPConf -> IndexReference -> Widget HTML Unit
homePageWidget conf indexReference = go Loading indexReference
  where 
    go widgetState reference = do
      res <- case widgetState of
        Default -> div [] []
        Loading -> loadingDiv <|> (Loaded <$> (liftAff $ runExceptT $ getIndex reference))
        Error err -> div [] [text err, simpleButton "Go back to login" false LogoutAction]
      case res of
        Loaded (Right index) -> void $ homePage index NoCard         
        Loaded (Left err) -> go (Error (show err)) reference
        LogoutAction -> pure unit
    
    homePage :: Index -> CardView -> Widget HTML HomePageAction
    homePage index cardReference = div [] [
      cardsManagerWidget conf index cardReference
    , simpleButton "Logout" false LogoutAction
    ]

