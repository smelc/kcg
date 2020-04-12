{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MainUi
  ( loadAssets,
    mainUI,
  )
where

import Board
import BoardUi
import Card
import CardUi (creatureID2AssetFilename)
import Constants
import Control.Exception
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Dynamic
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Juicy

data UIException
  = LoadException FilePath
  | CreaturePictNotFoundException CreatureID
  | CreatureLoadException CreatureID
  | InternalUnexpectedPictureType
  | InternalUnexpectedPictureTypeAt FilePath
  deriving (Show, Typeable)

instance Exception UIException

backgrounds :: NE.NonEmpty FilePath
backgrounds = (assetsGenPath ++ "/forest.png") NE.:| []

gameName = "Pixel Card Wars" -- Card Combat Retro

pictureSize ::
  Picture ->
  Maybe (Int, Int)
pictureSize picture =
  case picture of
    Bitmap bd -> Just $ bitmapSize bd
    BitmapSection rect _ -> Just $ rectSize rect
    whatever -> Nothing

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
  CreatureID ->
  IO (Maybe Picture)
loadCreature creatureID =
  loadJuicyPNG path
  where
    filename = creatureID2AssetFilename creatureID
    path = assetsGenPath ++ "/" ++ filename

-- | Loads backgrounds and creatures assets from disk
loadAssets ::
  (MonadIO m, MonadThrow m) =>
  [CreatureID] ->
  m Assets
loadAssets uiData = do
  bgs <- loadBackgrounds
  assocList <- liftIO $ traverse entryMaker uiData
  return $ Assets bgs $ Map.fromList assocList
  where
    entryMaker :: CreatureID -> IO (CreatureID, Picture)
    entryMaker id = do
      maybeV <- loadCreature id
      v <- getOrThrow maybeV $ CreatureLoadException id
      return (id, v)

-- | Builds the picture of a board
pictureBoard ::
  (MonadIO m, MonadThrow m) =>
  Assets ->
  Board ->
  m Picture
pictureBoard assets board =
  return $ mconcat (bg : cards')
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
    helper' (intCoord, creatureId) =
      case creaturePics assets Map.!? creatureId of
        Just pic -> pic
        Nothing -> throw $ CreaturePictNotFoundException creatureId
    cards' :: [Picture]
    cards' = map (helper' . Data.Bifunctor.second creatureId) cards

mainUI ::
  (MonadIO m, MonadThrow m) =>
  Assets ->
  m ()
mainUI assets = do
  pic <-
    pictureBoard
      assets
      $ Map.fromList [(PlayerBottom, botPlayer), (PlayerTop, topPlayer)]
  picSize <- getOrThrow (pictureSize pic) InternalUnexpectedPictureType
  liftIO $ display (InWindow gameName picSize (0, 0)) white pic
  where
    topPlayer = PlayerPart Map.empty Set.empty
    topTeam = Undead
    botPlayer = PlayerPart Map.empty Set.empty
    botTeam = Human

-- | Loads a background and display it
mainUIDeprecated ::
  (MonadIO m, MonadThrow m) =>
  m ()
mainUIDeprecated = do
  maybeBG <- liftIO $ loadJuicyPNG bgPath
  bg <- getOrThrow maybeBG $ LoadException bgPath
  bgSize <- getOrThrow (pictureSize bg) $ InternalUnexpectedPictureTypeAt bgPath
  liftIO $ display (InWindow gameName bgSize (0, 0)) white bg
  where
    bgPath :: FilePath = NE.head backgrounds
