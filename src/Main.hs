module Main where

import Control.Monad
import qualified Data.Colour.RGBSpace as C
import qualified Data.Colour.RGBSpace.HSL as C
import Data.List
import Data.Maybe
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

data State =
  State
    { alive :: S.Set Cell
    , adjacent :: Cell -> [Cell]
    }

potentialCells :: State -> [Cell]
potentialCells State {alive = alive, adjacent = adjacent} =
  uniq . sort . concatMap adjacent . S.toList $ alive

neighbours :: State -> Cell -> Int
neighbours State {alive = alive, adjacent = adjacent} =
  length . filter (`S.member` alive) . adjacent

nextState :: State -> State
nextState s = s {alive = S.fromList . filter f . potentialCells $ s}
  where
    f x = neighbours s x == 3 || (neighbours s x == 2 && x `S.member` alive s)

gameOfLife :: State -> [State]
gameOfLife = iterate nextState

-- Universe parameters
xmin :: Int
xmin = -100

xmax :: Int
xmax = 100

ymin :: Int
ymin = -100

ymax :: Int
ymax = 100

-- Adjacent functions
adjacentEuclidean :: Cell -> [Cell]
adjacentEuclidean (Cell (x, y)) =
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

adjacentTorus :: Cell -> [Cell]
adjacentTorus (Cell (x, y)) =
  Cell . wrap <$>
  [ (x - 1, y - 1)
  , (x - 1, y)
  , (x - 1, y + 1)
  , (x, y - 1)
  , (x, y + 1)
  , (x + 1, y - 1)
  , (x + 1, y)
  , (x + 1, y + 1)
  ]
  where
    wrap (x, y) =
      ( (x - xmin) `mod` (xmax - xmin + 1) + xmin
      , (y - ymin) `mod` (ymax - ymin + 1) + ymin)

-- Some interesting initial states
stateFromList :: (Cell -> [Cell]) -> [(Int, Int)] -> State
stateFromList adj l = State {alive = S.fromList $ Cell <$> l, adjacent = adj}

stateFromListEuclidean :: [(Int, Int)] -> State
stateFromListEuclidean = stateFromList adjacentEuclidean

blinker :: State
blinker = stateFromListEuclidean [(0, -1), (0, 0), (0, 1)]

toad :: State
toad = stateFromListEuclidean [(0, 1), (0, 2), (0, 3), (1, 0), (1, 1), (1, 2)]

randomState :: (Cell -> [Cell]) -> IO State
randomState adj =
  stateFromList adj <$>
  replicateM
    6000
    (liftM2
       (\x y ->
          ( (x `mod` (xmax - xmin + 1)) + xmin
          , (y `mod` (ymax - ymin + 1)) + ymin))
       randomIO
       randomIO)

-- Animation
cellDim :: Float
cellDim = 10

grid :: Picture
grid = pictures . map (color (greyN 0.4)) $ vLines ++ hLines
  where
    vLines =
      map
        ((\x ->
            line
              [ ((x - 0.5) * cellDim, (fromIntegral ymin - 0.5) * cellDim)
              , ((x - 0.5) * cellDim, (fromIntegral ymax + 0.5) * cellDim)
              ]) .
         fromIntegral)
        [xmin .. xmax + 1]
    hLines =
      map
        ((\y ->
            line
              [ ((fromIntegral xmin - 0.5) * cellDim, (y - 0.5) * cellDim)
              , ((fromIntegral xmax + 0.5) * cellDim, (y - 0.5) * cellDim)
              ]) .
         fromIntegral)
        [ymin .. ymax + 1]

colorFromRGB :: C.RGB Float -> Color
colorFromRGB c =
  makeColor (C.channelRed c) (C.channelGreen c) (C.channelBlue c) 1

cellColor :: Cell -> Color
cellColor (Cell (x, y)) =
  colorFromRGB
    (C.hsl
       (fromIntegral (x - xmin + ymax - y) /
        fromIntegral (xmax - xmin + ymax - ymin) *
        360)
       0.8
       0.6)

cellToPicture :: Cell -> Maybe Picture
cellToPicture c@(Cell (x, y))
  | xmin <= x && x <= xmax && ymin <= y && y <= ymax =
    Just
      (color (cellColor c) .
       translate (cellDim * fromIntegral x) (cellDim * fromIntegral y) $
       rectangleSolid cellDim cellDim)
  | otherwise = Nothing

drawState :: State -> Picture
drawState = pictures . mapMaybe cellToPicture . S.toList . alive

main :: IO ()
main = do
  s <- randomState adjacentTorus
  let states = gameOfLife s
  animate
    (InWindow "Conway's Game of Life" (300, 300) (0, 0))
    (greyN 0.1)
    (\x -> pictures [grid, drawState (states !! floor (10 * x))])
