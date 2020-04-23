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
  (MonadThrow m) =>
  Picture ->
  m (Float, Float)
pictureSize picture =
  case picture of
    Bitmap bd -> return $ both intToFloat $ bitmapSize bd
    BitmapSection rect _ -> return $ both intToFloat $ rectSize rect
    Pictures subs -> do
      subSizes <- traverse pictureSize subs
      return $ foldr sizeMax (0, 0) subSizes
      where
        sizeMax (i1, i2) (j1, j2) = (max i1 j1, max i2 j2)
    Scale xmult ymult pic -> do
      (subx, suby) <- pictureSize pic
      return (xmult * subx, ymult * suby)
    Translate x y pic -> do
      (subx, suby) <- pictureSize pic
      return (x + subx, y + suby)
    whatever -> throw $ InternalUnexpectedPictureType picture

data Assets
  = Assets
      { backgroundPics :: NE.NonEmpty Picture,
        creaturePics :: Map.Map CreatureID Picture
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

loadBackgrounds ::
  (MonadIO m, MonadThrow m) =>
  m (NE.NonEmpty Picture)
loadBackgrounds = do
  let pics = NE.map loadBackground backgrounds
  traverse Prelude.id pics
  where
    loadBackground :: (MonadIO m, MonadThrow m) => FilePath -> m Picture
    loadBackground filepath = do
      maybePic <- liftIO $ loadJuicyPNG filepath
      pic :: Picture <- getOrThrow maybePic $ LoadException filepath
      return pic

loadCreature ::
  (MonadIO m, MonadThrow m) =>
  CreatureID ->
  m Picture
loadCreature creatureID = do
  maybePic <- liftIO $ loadJuicyPNG path
  case maybePic of
    Nothing -> throw $ CreatureLoadException path creatureID
    Just pic -> return pic
  where
    path = creatureID2FilePath creatureID

-- | Loads backgrounds and creatures assets from disk
loadAssets ::
  (MonadIO m, MonadThrow m) =>
  [CreatureID] ->
  m Assets
loadAssets uiData = do
  bgs <- loadBackgrounds
  assocList <- liftIO $ traverse entryMaker uiData
  return $ Assets bgs $ Map.fromListWith handleDuplicate assocList
  where
    entryMaker :: CreatureID -> IO (CreatureID, Picture)
    entryMaker id = do
      v <- loadCreature id
      return (id, v)
    handleDuplicate = error "Duplicate keys found"

-- | Builds the picture of a board
pictureBoard ::
  Assets ->
  Board ->
  Picture
pictureBoard assets board =
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
    world = World board
    display' = InWindow gameName (800, 600) (0, 0)
    fps = 60
    drawer :: World -> Picture
    drawer World {board} = pictureBoard assets board
    stepper f w = w

mainUI ::
  (MonadIO m, MonadThrow m) =>
  Assets ->
  [Card UI] ->
  m ()
mainUI assets cards = do
  let pic = pictureBoard assets board
      pic' = Scale 0.66 0.66 pic
  picSize <- pictureSize pic'
  liftIO $ display (InWindow gameName (both ceiling picSize) (0, 0)) white pic'
  where
    board = exampleBoard cards

-- | Loads a background and display it
mainUIDeprecated ::
  (MonadIO m, MonadThrow m) =>
  m ()
mainUIDeprecated = do
  maybeBG <- liftIO $ loadJuicyPNG bgPath
  bg <- getOrThrow maybeBG $ LoadException bgPath
  bgSize <- pictureSize bg
  liftIO $ display (InWindow gameName (both ceiling bgSize) (0, 0)) white bg
  where
    bgPath :: FilePath = NE.head backgrounds
