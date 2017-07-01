{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module Snake where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import Data.Sequence (Seq, ViewL(..), ViewR(..), (<|))
import qualified Data.Sequence as S
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((&), (.~), (%~), (^.))
import Linear.V2 (V2(..), _x, _y)
import System.Random (Random(..), newStdGen)

-- Types

data Game = Game
  { _snake  :: Snake        -- ^ snake as a sequence of points in R2
  , _dir    :: Direction    -- ^ direction
  , _food   :: Coord        -- ^ location of the food
  , _foods  :: Stream Coord -- ^ infinite list of random next food locations
  , _dead   :: Bool         -- ^ game over flag
  , _paused :: Bool         -- ^ paused flag
  , _score  :: Int          -- ^ score
  , _frozen :: Bool         -- ^ freeze to disallow duplicate turns between time steps
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
width  = 20

-- Functions

-- | Step forward in time
step :: Game -> Game
step g = fromMaybe g $ do
  guard (not $ g ^. paused || g ^. dead)
  let g' = g & frozen .~ False
  return . fromMaybe (move g') $ die g' <|> eatFood g'

-- | Possibly die if next head position is disallowed
die :: Game -> Maybe Game
die g = do
  guard (nh g `elem` g ^. snake)
  return $ g & dead .~ True

-- | Possibly eat food if next head position is food
eatFood :: Game -> Maybe Game
eatFood g = do
  guard (nh g == g ^. food)
  let g' = g & score %~ (+10)
             & snake %~ (nh g <|)
  return (nextFood g')

-- | Set a valid next food coordinate
nextFood :: Game -> Game
nextFood g =
  let (f :| fs) = g ^. foods
   in if (f `elem` g ^. snake)
         then nextFood (g & foods .~ fs)
         else g & foods .~ fs
                & food  .~ f

-- | Move snake along in a marquee fashion
move :: Game -> Game
move g = g & snake %~ (mv . S.viewr)
  where
    mv (EmptyR) = error "Snakes can't be empty!"
    mv (s :> _) = nh g <| s

-- | Get next head location of the game's snake
nh :: Game -> Coord
nh g = nextHead (g ^. dir) (g ^. snake)

-- | Get next head position of a snake in a particular direction
nextHead :: Direction -> Snake -> Coord
nextHead d = go . S.viewl
  where
    go (EmptyL) = error "Snakes can't be empty!"
    go (a :< _)
      | d == North = a & _y %~ (\y -> (y + 1) `mod` height)
      | d == South = a & _y %~ (\y -> (y - 1) `mod` height)
      | d == East  = a & _x %~ (\x -> (x + 1) `mod` width)
      | d == West  = a & _x %~ (\x -> (x - 1) `mod` width)

-- | Turn game direction (only turns orthogonally)
--
-- Implicitly unpauses yet freezes game
turn :: Direction -> Game -> Game
turn d g =
  if g ^. frozen
     then g
     else g & dir %~ (turnDir d)
            & paused .~ False
            & frozen .~ True

turnDir :: Direction -> Direction -> Direction
turnDir n c
  | c `elem` [North, South] && n `elem` [East, West] = n
  | c `elem` [East, West] && n `elem` [North, South] = n
  | otherwise                             = c

-- | Initialize a paused game with random food location
initGame :: IO Game
initGame = do
  (f :| fs) <- fromList . randomRs (V2 0 0, V2 (width - 1) (height - 1)) <$> newStdGen
  let xm = width `div` 2
      ym = height `div` 2
      g  = Game { _snake = (S.singleton (V2 xm ym))
                , _food = f, _foods = fs
                , _score = 0
                , _dir = North
                , _dead = False, _paused = True , _frozen = False }
  return $ nextFood g

instance Random a => Random (V2 a) where
  randomR (V2 x1 y1, V2 x2 y2) g =
    let (x, g')  = randomR (x1, x2) g
        (y, g'') = randomR (y1, y2) g'
     in (V2 x y, g'')
  random g =
    let (x, g')  = random g
        (y, g'') = random g'
     in (V2 x y, g'')

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")
