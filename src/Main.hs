module Main where

import Control.Monad
import Data.List
import qualified Data.Set as S
import Graphics.Gloss
import System.Random

uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq [x] = [x]
uniq (x:y:xs) =
  if x == y
    then uniq (x : xs)
    else x : uniq xs

-- Game Logic
newtype Cell =
  Cell (Int, Int)
  deriving (Eq, Ord, Show)

newtype State =
  State
    { alive :: S.Set Cell
    }
  deriving (Eq, Show)

adjacent :: Cell -> [Cell]
adjacent (Cell (x, y)) =
  Cell <$>
  [ (x - 1, y - 1)
  , (x - 1, y)
  , (x - 1, y + 1)
  , (x, y - 1)
  , (x, y + 1)
  , (x + 1, y - 1)
  , (x + 1, y)
  , (x + 1, y + 1)
  ]

potentialCells :: State -> [Cell]
potentialCells = uniq . sort . concatMap adjacent . S.toList . alive

neighbours :: State -> Cell -> Int
neighbours State {alive = alive} = length . filter (`S.member` alive) . adjacent

nextState :: State -> State
nextState s = State {alive = S.fromList . filter f . potentialCells $ s}
  where
    f x = neighbours s x == 3 || (neighbours s x == 2 && x `S.member` alive s)

gameOfLife :: State -> [State]
gameOfLife = iterate nextState

stateFromList :: [(Int, Int)] -> State
stateFromList l = State {alive = S.fromList $ Cell <$> l}

-- Some interesting initial states
blinker :: State
blinker = stateFromList [(0, -1), (0, 0), (0, 1)]

toad :: State
toad = stateFromList [(0, 1), (0, 2), (0, 3), (1, 0), (1, 1), (1, 2)]

randomState :: IO State
randomState =
  stateFromList <$>
  replicateM
    5000
    (liftM2 (\x y -> ((x `mod` 100) - 50, (y `mod` 100) - 50)) randomIO randomIO)

-- Animation
cellDim :: Float
cellDim = 10

grid :: Picture
grid = pictures (vLines ++ hLines)
  where
    vLines =
      map
        (\x -> line [((x + 0.5) * cellDim, -1000), ((x + 0.5) * cellDim, 1000)])
        [-100 .. 100]
    hLines =
      map
        (\y -> line [(-1000, (y + 0.5) * cellDim), (1000, (y + 0.5) * cellDim)])
        [-100 .. 100]

drawState :: State -> Picture
drawState =
  pictures .
  map
    (\(Cell (x, y)) ->
       translate
         (cellDim * fromIntegral x)
         (cellDim * fromIntegral y)
         (rectangleSolid cellDim cellDim)) .
  S.toList . alive

main :: IO ()
main = do
  s <- randomState
  let states = gameOfLife s
  animate
    (InWindow "Conway's Game of Life" (300, 300) (0, 0))
    white
    (\x -> pictures [grid, drawState (states !! floor (2 * x))])
