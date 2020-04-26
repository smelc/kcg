{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MainUi
  ( loadAssets,
    mainPlay,
    mainUI,
  )
where

import Board
import BoardUi
import Card
import CardUi (creatureID2FilePath)
import Constants
import Control.Exception
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Dynamic
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as Set
import Data.Tuple.Extra (both)
import Debug.Trace
import GHC.Stack (HasCallStack)
import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Juicy
import Numeric.Extra (intToFloat)
import World

data UIException
  = LoadException FilePath
  | CreaturePictNotFoundException CreatureID
  | CreatureLoadException FilePath CreatureID
  | InternalUnexpectedPictureType Picture
  deriving (Show, Typeable)

instance Exception UIException

backgrounds :: NE.NonEmpty FilePath
backgrounds = (assetsGenPath ++ "/forest.png") NE.:| []

gameName = "Pixel Card Wars" -- Card Combat Retro

pictureSize ::
  HasCallStack =>
  Picture ->
  (Float, Float)
pictureSize picture =
  case picture of
    Bitmap bd -> both intToFloat $ bitmapSize bd
    BitmapSection rect _ -> both intToFloat $ rectSize rect
    Pictures subs -> do
      let subSizes = map pictureSize subs
      foldr sizeMax (0, 0) subSizes
      where
        sizeMax (i1, i2) (j1, j2) = (max i1 j1, max i2 j2)
    Scale xmult ymult pic -> do
      let (subx, suby) = pictureSize pic
      (xmult * subx, ymult * suby)
    Translate x y pic -> do
      let (subx, suby) = pictureSize pic
      (x + subx, y + suby)
    whatever -> throw $ InternalUnexpectedPictureType picture

data Assets
  = Assets
      { backgroundPics :: NE.NonEmpty Picture,
        creaturePics :: Map.Map CreatureID Picture,
        cardOverlay :: Picture
      }

getOrThrow ::
  (MonadThrow m) =>
  Maybe a ->
  UIException ->
  m a
getOrThrow ma e =
  case ma of
    Nothing -> throw e
    Just a -> return a

loadOrThrowJuicyPNG :: (MonadIO m, MonadThrow m) => FilePath -> m Picture
loadOrThrowJuicyPNG filepath = do
  maybePic <- liftIO $ loadJuicyPNG filepath
  pic :: Picture <- getOrThrow maybePic $ LoadException filepath
  return pic

loadBackgrounds ::
  (MonadIO m, MonadThrow m) =>
  m (NE.NonEmpty Picture)
loadBackgrounds = do
  let pics = NE.map loadOrThrowJuicyPNG backgrounds
  traverse Prelude.id pics

loadCreature ::
  (MonadIO m, MonadThrow m) =>
  CreatureID ->
  m Picture
loadCreature creatureID =
  loadOrThrowJuicyPNG $ creatureID2FilePath creatureID

-- | Loads backgrounds and creatures assets from disk
loadAssets ::
  (MonadIO m, MonadThrow m) =>
  [CreatureID] ->
  m Assets
loadAssets uiData = do
  bgs <- loadBackgrounds
  assocList <- liftIO $ traverse entryMaker uiData
  cardOverlay <- loadOrThrowJuicyPNG $ assetsGenPath ++ "/" ++ "card-overlay.png"
  return $
    Assets
      bgs
      (Map.fromListWith handleDuplicate assocList)
      cardOverlay
  where
    entryMaker :: CreatureID -> IO (CreatureID, Picture)
    entryMaker id = do
      v <- loadCreature id
      return (id, v)
    handleDuplicate = error "Duplicate keys found"

-- | Builds the picture of a board
pictureBoard ::
  HasCallStack =>
  Assets ->
  (Board, Maybe Event) ->
  Picture
pictureBoard assets (board, maybeEvent) =
  mconcat (bg : cards')
  where
    bg :: Picture = NE.head $ backgroundPics assets
    board' :: [(PlayerSpot, [(CardSpot, Creature Core)])]
    board' = Map.toList $ Map.map (Map.toList . visible) board
    helper :: PlayerSpot -> (CardSpot, a) -> (IntCoord, a)
    helper p (c, w) = (cardPixelsOffset p c, w)
    cards :: [(IntCoord, Creature Core)]
    cards =
      concatMap
        ( \(playerSpot, spotsAndCreatures :: [(CardSpot, Creature Core)]) ->
            map (helper playerSpot) spotsAndCreatures
        )
        board'
    helper' ((xoffset, yoffset), creatureId) =
      case creaturePics assets Map.!? creatureId of
        Just pic -> Translate (intToFloat xoffset) (intToFloat yoffset) pic
        Nothing -> throw $ CreaturePictNotFoundException creatureId
    cards' = map (helper' . Data.Bifunctor.second creatureId) cards

humanGeneral = CreatureID General Human

humanSpearman = CreatureID Spearman Human

undeadArcher = CreatureID Archer Undead

undeadMummy = CreatureID Mummy Undead

undeadVampire = CreatureID Vampire Undead

exampleBoard :: [Card UI] -> Board
exampleBoard cards =
  Map.fromList [(PlayerBottom, botPlayer), (PlayerTop, topPlayer)]
  where
    creatures :: [Creature Core] =
      map creatureUI2CreatureCore $ mapMaybe card2Creature cards
    getCardByID searched =
      head $ filter (\c -> creatureId c == searched) creatures
    hGeneral = getCardByID humanGeneral
    hSpearman = getCardByID humanSpearman
    udArcher = getCardByID undeadArcher
    udMummy = getCardByID undeadMummy
    udVampire = getCardByID undeadVampire
    topPlayer = PlayerPart topCards Set.empty
    topCards :: CardsOnTable =
      listToCardsOnTable
        [ Nothing,
          Just udArcher,
          Nothing,
          Nothing,
          Just udVampire,
          Just udMummy
        ]
    botPlayer = PlayerPart botCards Set.empty
    botCards :: CardsOnTable =
      listToCardsOnTable
        [ Nothing,
          Just hGeneral,
          Just hSpearman
        ]

eventHandler :: Event -> World -> World
eventHandler e w =
  trace ("Handling " ++ show e) w

mainPlay :: (MonadIO m, MonadThrow m) => Assets -> [Card 'UI] -> m ()
mainPlay assets cards =
  liftIO $ play display' white fps world drawer eventHandler stepper
  where
    board = exampleBoard cards
    boardPicture = pictureBoard assets (board, Nothing)
    world = World board
    (framex, framey) = pictureSize boardPicture
    display' = InWindow gameName (round framex, round framey) (0, 0)
    fps = 60
    drawer :: World -> Picture
    drawer World {board} = boardPicture
    stepper f w = w

mainUI ::
  (MonadIO m, MonadThrow m) =>
  Assets ->
  [Card UI] ->
  m ()
mainUI assets cards = do
  let pic = pictureBoard assets (board, Nothing)
      pic' = Scale 0.66 0.66 pic
      picSize = pictureSize pic'
  liftIO $ display (InWindow gameName (both ceiling picSize) (0, 0)) white pic'
  where
    board = exampleBoard cards
