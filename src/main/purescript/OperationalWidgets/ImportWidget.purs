module OperationalWidgets.ImportWidget where

import Concur.Core (Widget)
import Concur.Core.FRP (Signal, fireOnce, demand, loopW, loopS, step, dyn, hold)
import Concur.React (HTML)
import Concur.React.DOM (text, div, h1, form, h3, ul, li', p)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Control.Monad.Except.Trans (runExceptT, ExceptT(..), except)
import Control.Semigroupoid ((<<<))
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (filter, length, cons)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), isLeft)
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.HeytingAlgebra (not)
import Data.List (List(..), (:), concat, fromFoldable)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), snd)
import Data.Unit (unit)
import DataModel.AppState (AppError)
import DataModel.Card (Card(..), CardValues(..))
import DataModel.Index (Index(..), CardEntry)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Functions.Communication.Cards (postCard)
import Functions.Communication.Users (updateIndex)
import Functions.Import (decodeImport, parseHTMLImport, decodeHTML)
import Functions.Time (getCurrentDateTime, formatDateTimeToDate)
import Views.CardViews (cardField)
import Views.SimpleWebComponents (loadingDiv, simpleFileInputWidget, simpleTextInputWidget, simpleTextAreaSignal, simpleCheckboxSignal, simpleButton)

data QuickSelection = All | None | Archived | NonArchived

data SelectionAction = Cards (Array (Tuple Boolean Card)) | NewQuickSelection QuickSelection

data ImportStep = UploadContent String | ChooseCards (Array (Tuple Boolean Card)) | Confirm (Array (Tuple Boolean Card))

importWidget :: Index -> Widget HTML (Either AppError Index)
importWidget index@(Index entries) = div [Props._id "importPage"] [h1 [] [text "Import"], importPage Nothing (UploadContent "")]

  where 
    saveImport :: Array Card -> Aff (Either AppError Index)
    saveImport cards = runExceptT $ do
      newEntries :: Array CardEntry <- sequence (postCard <$> cards)
      let newIndex = Index (concat $ entries : (fromFoldable newEntries) : Nil)
      _ <- updateIndex newIndex
      ExceptT $ pure $ Right newIndex

    importPage :: Maybe String -> ImportStep -> Widget HTML (Either AppError Index)
    importPage error (UploadContent pl) = do
      res <- div [] [
          text (fromMaybe "" error)
        , p [] [text "Import data from another Clipperz account using a JSON/HTML export file created by Clipperz."]
        , div [] [
            Left <$> (simpleFileInputWidget "import" (text "Import"))
          , p [] [text "Alternatively you may type or paste any properly formatted JSON data."]
          , Right <$> (demand $ do
                                textContent <- simpleTextAreaSignal "importText" (text "Import") "Type or copy your data here" pl
                                fireOnce (simpleButton "Import" (isLeft (jsonParser textContent)) textContent))
          ]
        ]
      case res of
        Left html -> do
          eitherCards <- runExceptT $ do
            codedCardData <- except $ parseHTMLImport html
            decodedCardData <- except $ Right $ decodeHTML codedCardData
            ExceptT $ liftEffect $ decodeImport decodedCardData
          case eitherCards of
            Left err -> importPage (Just $ show err) (UploadContent "")
            Right cards -> importPage Nothing $ ChooseCards $ (\c@(Card r) -> Tuple (not r.archived) c) <$> cards
        Right jsonText ->
          let eitherJson = jsonParser jsonText
          in case eitherJson of
              Left err -> importPage (Just $ show err) (UploadContent jsonText)
              Right json ->
                let eitherCards = decodeJson json
                in case eitherCards of
                    Left err -> importPage (Just $ show err) (UploadContent (stringify json))
                    Right cards -> importPage Nothing $ ChooseCards $ (\c@(Card r) -> Tuple (not r.archived) c) <$> cards
    importPage _ (ChooseCards cards) = do
      (cardSelectionWidget (UploadContent (stringify $ encodeJson (snd <$> cards))) cards) >>= (importPage Nothing)
    importPage _ (Confirm cards) = 
      let total = length cards
          toImportCards = snd <$> (filter (\(Tuple b _) -> b) cards)
          toImport = length toImportCards
      in do
          res <- div [] [
            text $ "Import " <> (show toImport) <> " cards (of " <> (show total) <> ")?"
          , ((simpleButton "<<" false false) <|> (simpleButton "Import" false true))
          ]
          if res then loadingDiv <|> (liftAff $ saveImport toImportCards)
          else importPage Nothing (ChooseCards cards) 

    cardSelectionWidget :: ImportStep -> Array (Tuple Boolean Card) -> Widget HTML ImportStep --(Array (Tuple Boolean Card))
    cardSelectionWidget goBackValue cards = do
      newTagWithDate <- (((<>) "Import_") <<< formatDateTimeToDate) <$> (liftEffect getCurrentDateTime)
      res <- div [Props.className "scrollable"] [ 
          Left <$> selectWidget 
        , Right <$> (demand $ do
            newTag <- loopS { tag: newTagWithDate, cb: true } $ \v -> do
                                                            newTagCB <- simpleCheckboxSignal "addTag" (text "Apply the following tag to imported cards:") true v.cb
                                                            newTag <- loopW v.tag (simpleTextInputWidget "newTag" (text "Tag") "Tag")
                                                            pure $ { tag: newTag, cb: newTagCB }
            selectedCards <- sequence $ importCardProposalWidget <$> cards
            let preparedCards = if newTag.cb then prepareSelectedCards newTag.tag selectedCards else selectedCards
            fireOnce (div [Props.className "fixedFoot"] [(simpleButton "<<" false goBackValue) <|> (simpleButton "Import" false (Confirm preparedCards))]))
        ]
      case res of
        Left sel -> cardSelectionWidget goBackValue $ filterCards sel cards
        Right value -> pure value

    filterCards :: QuickSelection -> Array (Tuple Boolean Card) -> Array (Tuple Boolean Card)
    filterCards All arr = (\(Tuple _ c) -> Tuple true c) <$> arr
    filterCards None arr = (\(Tuple _ c) -> Tuple false c) <$> arr
    filterCards Archived arr = (\(Tuple _ c@(Card r)) -> Tuple r.archived c) <$> arr
    filterCards NonArchived arr = (\(Tuple _ c@(Card r)) -> Tuple (not r.archived) c) <$> arr

    selectWidget :: Widget HTML QuickSelection
    selectWidget = div [] [
      p [] [text "Select:"]
    , simpleButton "All" false All
    , simpleButton "None" false None
    , simpleButton "Archived" false Archived
    , simpleButton "Not archived" false NonArchived
    ]

    importCardProposalWidget :: Tuple Boolean Card -> Signal HTML (Tuple Boolean Card)
    importCardProposalWidget (Tuple b c@(Card { content: cv@(CardValues content), archived, timestamp})) = do
      cb <- simpleCheckboxSignal ("importCheckbox_" <> content.title) (cardContent cv) true b
      pure $ Tuple cb c

    cardContent :: forall a. CardValues -> Widget HTML a
    cardContent (CardValues {title: t, tags: ts, fields: fs, notes: n}) = div [Props.className "cardContent"] [
      h3  [Props.className "card_title"]  [text t]
    , ul  [Props.className "card_tags"]   $ (\s -> li' [text s]) <$> ts
    , div [Props.className "card_fields"] $ cardField <$> fs
    , div [Props.className "card_notes"]  [text n]
    ]

    prepareSelectedCards :: String -> Array (Tuple Boolean Card) -> Array (Tuple Boolean Card)
    prepareSelectedCards newTag =
      (<$>) (\(Tuple b (Card { content: CardValues r, archived, timestamp})) -> Tuple b (Card { archived, timestamp, content: (CardValues $ r { tags = (cons newTag r.tags) })}))
