{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MainUi
  ( loadAssets,
    mainPlay,
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
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Tuple.Extra (both)
import Debug.Trace
import Event
import GHC.Stack (HasCallStack)
import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Juicy
import Numeric.Extra (intToFloat)
import UtilUi
import World

backgrounds :: NE.NonEmpty FilePath
backgrounds = (assetsGenPath ++ "/forest.png") NE.:| []

gameName = "Pixel Card Wars" -- Card Combat Retro

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
pictureWorld ::
  HasCallStack =>
  Assets ->
  World ->
  Picture
pictureWorld assets@Assets {cardOverlay} World {board, overlaid} =
  mconcat (bg : cards')
  where
    bg :: Picture = NE.head $ backgroundPics assets
    board' :: [(PlayerSpot, [(CardSpot, Creature Core)])]
    board' = Map.toList $ Map.map (Map.toList . visible) board
    board'' :: [(PlayerSpot, CardSpot, Creature 'Core)]
    board'' =
      concatMap
        (\(playerSpot, sublist) -> [(playerSpot, cardSpot, creature) | (cardSpot, creature) <- sublist])
        board'
    helper :: PlayerSpot -> (CardSpot, a) -> (IntCoord, a)
    helper p (c, w) = (cardPixelsOffset p c, w)
    cards :: [(IntCoord, Creature Core)]
    cards =
      concatMap
        ( \(playerSpot, spotsAndCreatures :: [(CardSpot, Creature Core)]) ->
            map (helper playerSpot) spotsAndCreatures
        )
        board'
    helper' :: (IntCoord, CreatureID) -> Picture
    helper' ((xoffset, yoffset), creatureId) =
      case creaturePics assets Map.!? creatureId of
        Just pic ->
          let (picx, picy) = (intToFloat xoffset, intToFloat yoffset)
           in Translate picx picy pic
        Nothing -> throw $ CreaturePictNotFoundException creatureId
    cards' :: [Picture]
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
eventHandler e world@World {board, overlaid} =
  let allSpots = [(p, c, cardPixelsOffset p c) | p <- allPlayersSpots, c <- allCardsSpots]
      allSpots' = map (\(p, c, intCoord) -> (p, c, both intToFloat intCoord)) allSpots
      hoveredSpots =
        filter
          ( \(_, _, rectCenter) ->
              isEventIn
                e
                (both intToFloat cardPixelsSize)
                rectCenter
          )
          allSpots'
      hoveredSpot = listToMaybe hoveredSpots
      hoveredSpot' = fmap (\(f, s, _) -> (f, s)) hoveredSpot
   in trace ("Handling " ++ show e) World board hoveredSpot'

mainPlay :: (MonadIO m, MonadThrow m) => Assets -> [Card 'UI] -> m ()
mainPlay assets cards =
  liftIO $ play display' white fps world drawer eventHandler stepper
  where
    board = exampleBoard cards
    boardPicture = pictureWorld assets $ World board Nothing
    world = World board undefined
    (framex, framey) = pictureSize boardPicture
    display' = InWindow gameName (round framex, round framey) (0, 0)
    fps = 60
    drawer :: World -> Picture
    drawer = pictureWorld assets
    stepper f w = w
