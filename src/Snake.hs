{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Snake
  ( initGame
  , step
  , turn
  , Game(..)
  , Direction(..)
  , dead, food, score, snake
  , height, width
  ) where

import Control.Applicative ((<|>))
import Control.Monad (guard, void)
import Data.Maybe (fromMaybe)

import Control.Lens hiding ((<|), (|>), (:>), (:<), index)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Class (MonadState, modify, get)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State (execState)
import Control.Monad.Extra (orM, unlessM)
import Data.Sequence (Seq(..), (<|), index)
import qualified Data.Sequence as S
import Linear.V2 (V2(..), _x, _y)
import System.Random (Random(..), newStdGen)

-- Types

data Game = Game
  { _snake  :: Snake        -- ^ snake as a sequence of points in N2
  , _dir    :: Direction    -- ^ direction
  , _food   :: Coord        -- ^ location of the food
  , _foods  :: Stream Coord -- ^ infinite list of random next food locations
  , _dead   :: Bool         -- ^ game over flag
  , _paused :: Bool         -- ^ paused flag
  , _score  :: Int          -- ^ score
  , _locked :: Bool         -- ^ lock to disallow duplicate turns between time steps
  } deriving (Show)

type Coord = V2 Int

type Snake = Seq Coord

data Stream a = a :| Stream a
  deriving (Show)

data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

makeLenses ''Game

-- Constants

height, width :: Int
height = 20
width = 20

-- Functions

-- | Step forward in time
step :: MonadState Game m => m ()
step = void . runMaybeT $ do

  -- Make sure the game isn't paused or over
  MaybeT $ guard . not <$> orM [use paused, use dead]

  -- Unlock from last directional turn
  MaybeT . fmap Just $ locked .= False

  -- die (moved into self), eat (moved into food), or move (move into space)
  die <|> eatFood <|> MaybeT (Just <$> move)

-- | Possibly die if next head position is in snake
die :: MonadState Game m => MaybeT m ()
die = do
  MaybeT . fmap guard $ elem <$> nextHead <*> (use snake)
  MaybeT . fmap Just $ dead .= True

-- | Possibly eat food if next head position is food
eatFood :: MonadState Game m => MaybeT m ()
eatFood = do
  MaybeT . fmap guard $ (==) <$> nextHead <*> (use food)
  MaybeT . fmap Just $ do
    modifying score (+ 10)
    nh <- nextHead
    modifying snake (nh <|)
    nextFood

-- | Set a valid next food coordinate
nextFood :: MonadState Game m => m ()
nextFood = do
  (f :| fs) <- use foods
  foods .= fs
  elem f <$> use snake >>= \case
    True -> nextFood
    False -> food .= f

-- | Move snake along in a marquee fashion
move :: MonadState Game m => m ()
move = do
  nh <- nextHead
  modifying snake $ \(s :|> _) -> nh <| s

-- | Get next head position of the snake
nextHead :: MonadState Game m => m Coord
nextHead = get <&> \(Game {_snake = (a :<| _), _dir = d}) -> case d of
  North -> a & _y %~ (\y -> (y + 1) `mod` height)
  South -> a & _y %~ (\y -> (y - 1) `mod` height)
  East  -> a & _x %~ (\x -> (x + 1) `mod` width)
  West  -> a & _x %~ (\x -> (x - 1) `mod` width)

-- | Turn game direction (only turns orthogonally)
--
-- Implicitly unpauses yet locks game
turn :: MonadState Game m => Direction -> m ()
turn d = do
  unlessM (use locked) $ do
    modifying dir (turnDir d)
    assign paused False
    assign locked True

turnDir :: Direction -> Direction -> Direction
turnDir n c | c `elem` [North, South] && n `elem` [East, West] = n
            | c `elem` [East, West] && n `elem` [North, South] = n
            | otherwise = c

-- | Initialize a paused game with random food location
initGame :: MonadIO m => m Game
initGame = do
  (f :| fs) <-
    fromList . randomRs (V2 0 0, V2 (width - 1) (height - 1)) <$> newStdGen
  let xm = width `div` 2
      ym = height `div` 2
      g  = Game
        { _snake  = (S.singleton (V2 xm ym))
        , _food   = f
        , _foods  = fs
        , _score  = 0
        , _dir    = North
        , _dead   = False
        , _paused = True
        , _locked = False
        }
  pure $ execState nextFood g

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")
