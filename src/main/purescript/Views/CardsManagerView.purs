module Views.CardsManagerView where


import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text)
import Concur.React.Props as Props
import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (fromMaybe)
import DataModel.AppState (AppError)
import DataModel.Card (Card)
import DataModel.Index (CardReference, Index)
import DataModel.WidgetOperations (IndexUpdateAction(..), IndexUpdateData(..))
import DataModel.WidgetState (WidgetState(..))
import Views.CardViews (cardView)
import Views.CreateCardView (createCardView)
import Views.IndexView (indexView)
import Views.SimpleWebComponents (simpleButton, loadingDiv)
import OperationalWidgets.CardWidget (cardWidget)
import OperationalWidgets.CreateCardWidget (createCardWidget)

data CardViewAction = UpdateIndex IndexUpdateData | ShowCard CardReference | ShowAddCard
instance showCardViewAction :: Show CardViewAction where
  show (UpdateIndex (IndexUpdateData a _)) = "UpdateIndex " <> show a
  show (ShowCard ref)  = "Show Card " <> show ref
  show  ShowAddCard    = "Show Add Card"

type CardViewState = { cardView :: CardView, cardViewState :: WidgetState }

data CardView = NoCard | CardFromReference CardReference | JustCard Card | CardForm Card
instance showCardView :: Show CardView where
  show NoCard = "NoCard"
  show (CardFromReference cr) = "CardFromReference " <> show cr
  show (JustCard c) = "JustCard " <> show c
  show (CardForm c) = "CardForm " <> show c

cardsManagerView :: Index -> CardViewState -> Maybe AppError -> Widget HTML CardViewAction
cardsManagerView i cvs@{ cardView: cv, cardViewState } error = 
  let disableIndex = case cv of
                      CardForm _ -> true
                      _          -> false
  in do 
    res <- div [Props._id "cardsManager"] $ (text <$> (fromMaybe $ show <$> error)) <> [
      div [Props._id "indexView"] [
        ShowCard <$> indexView disableIndex i
      , simpleButton "Add card" disableIndex ShowAddCard 
      ]
    , div [Props._id "cardView"] $
      case cvs of
        { cardView: CardForm card,         cardViewState: Loading } -> [(UpdateIndex $ IndexUpdateData NoUpdate card) <$ createCardView card cardViewState]
        { cardView: CardForm card,         cardViewState: _ }       -> [UpdateIndex <$> createCardWidget card cardViewState]
        { cardView: CardFromReference ref, cardViewState: _ }       -> [UpdateIndex <$> cardWidget ref cardViewState]
        { cardView: JustCard card,         cardViewState: Loading } -> [(UpdateIndex $ IndexUpdateData NoUpdate card) <$ (div [] [loadingDiv, cardView card])]
        { cardView: JustCard card,         cardViewState: _ }       -> [(UpdateIndex $ IndexUpdateData NoUpdate card) <$ cardView card]
        { cardView: NoCard       ,         cardViewState: _ }       -> []
    ]
    case res of
      ShowCard ref -> cardsManagerView i { cardView: CardFromReference ref, cardViewState } Nothing -- TODO: discuss
      _ -> pure res

