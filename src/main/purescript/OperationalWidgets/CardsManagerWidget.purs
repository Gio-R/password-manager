module OperationalWidgets.CardsManagerWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (runExceptT)
import Data.Either (Either(..))
import Data.Eq ((/=))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.List ((:), filter)
import Data.Maybe (Maybe(..))
import Data.Show (show)
import DataModel.AppState (AppError(..), InvalidStateError(..))
import DataModel.Card (emptyCard)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Index (Index(..), CardEntry(..))
import DataModel.WidgetOperations (IndexUpdateAction(..), IndexUpdateData(..))
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Functions.Communication.Cards (updateIndex)
import Functions.SRP as SRP
import Views.CardsManagerView (cardsManagerView, CardView(..), CardViewAction(..), CardViewState)

data CardsViewResult = CardsViewResult CardViewAction | OpResult Index CardViewState (Maybe AppError)

cardsManagerWidget :: forall a. SRP.SRPConf -> Index -> CardViewState -> Widget HTML a
cardsManagerWidget conf ind cardViewState = go ind (cardsManagerView ind cardViewState) Nothing Nothing

  where
    go :: Index -> (Maybe AppError -> Widget HTML CardViewAction) -> Maybe AppError -> Maybe (Aff CardsViewResult) -> Widget HTML a
    go index view mError operation = do
      res <- case operation of
        Nothing -> CardsViewResult <$> (view mError)
        Just op -> (CardsViewResult <$> (view mError)) <|> (liftAff $ op)
      case res of
        CardsViewResult cva -> case cva of 
          UpdateIndex updateData -> do
            _ <- log $ show updateData
            go index (getUpdateIndexView index updateData) Nothing (Just (getUpdateIndexOp conf index updateData))
          ShowAddCard -> go index (cardsManagerView index {cardView: (CardForm emptyCard), cardViewState: Default}) Nothing Nothing
          ShowCard ref -> go index (cardsManagerView index {cardView: (CardFromReference ref), cardViewState: Default}) Nothing Nothing
        OpResult i cv e -> go i (cardsManagerView i cv) e Nothing

getUpdateIndexOp :: SRP.SRPConf -> Index -> IndexUpdateData -> Aff CardsViewResult
getUpdateIndexOp conf index@(Index_v1 list) (IndexUpdateData action _) =
  case action of 
    AddReference                        entry -> addEntryToIndex entry 
    CloneReference                      entry -> addEntryToIndex entry
    ChangeReferenceWithEdit    oldEntry entry -> updateReferenceInIndex oldEntry entry
    ChangeReferenceWithoutEdit oldEntry entry -> updateReferenceInIndex oldEntry entry
    DeleteReference            oldEntry       -> removeReferenceFromIndex oldEntry
    _ -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } Nothing

  where
    addEntryToIndex entry = do
      let newIndex = Index_v1 (entry : list)
      manageUpdateIndex newIndex { cardView: (CardFromReference entry), cardViewState: Default }
    
    removeReferenceFromIndex (CardEntry_v1 { cardReference: reference }) = do
      let newIndex = Index_v1 (filter (\(CardEntry_v1 { cardReference }) -> cardReference /= reference) list)
      manageUpdateIndex newIndex { cardView: NoCard, cardViewState: Default }

    updateReferenceInIndex (CardEntry_v1 { cardReference: reference }) entry = do --TODO finish implementation based on card versioning
      let newIndex = Index_v1 (entry : filter (\(CardEntry_v1 { cardReference }) -> cardReference /= reference) list)
      manageUpdateIndex newIndex { cardView: (CardFromReference entry), cardViewState: Default }

    manageUpdateIndex :: Index -> CardViewState -> Aff CardsViewResult
    manageUpdateIndex newIndex cardViewState = do
      updateResult <- liftAff $ runExceptT $ updateIndex conf newIndex
      case updateResult of
        Right _   -> pure $ OpResult newIndex cardViewState Nothing
        Left  err -> 
          case err of 
            InvalidStateError (CorruptedState  s) -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err)
            InvalidStateError (MissingValue    s) -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err)
            ProtocolError     (RequestError    e) -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err)
            ProtocolError     (ResponseError   i) -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err)
            ProtocolError     (SRPError        s) -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err)
            ProtocolError     (DecodeError     s) -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err)
            ProtocolError     (CryptoError     s) -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err)
            ProtocolError     (IllegalRequest  s) -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err)
            ProtocolError     (IllegalResponse s) -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err)

getUpdateIndexView :: Index -> IndexUpdateData -> (Maybe AppError -> Widget HTML CardViewAction)
getUpdateIndexView index (IndexUpdateData action card) = 
  case action of 
    AddReference                 _ -> cardsManagerView index { cardView: (CardForm card), cardViewState: Loading } 
    CloneReference               _ -> cardsManagerView index { cardView: (JustCard card), cardViewState: Loading }
    DeleteReference              _ -> cardsManagerView index { cardView: (JustCard card), cardViewState: Loading }
    ChangeReferenceWithEdit    _ _ -> cardsManagerView index { cardView: (CardForm card), cardViewState: Loading } 
    ChangeReferenceWithoutEdit _ _ -> cardsManagerView index { cardView: (JustCard card), cardViewState: Loading } 
    _                     -> cardsManagerView index { cardView:  NoCard,         cardViewState: Default }
