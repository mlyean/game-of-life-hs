module Main where

import Control.Monad
import Control.Monad.Trans.State.Strict
import qualified Data.Colour.RGBSpace as C
import qualified Data.Colour.RGBSpace.HSL as C
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Simulate
import System.Random

-- Utility functions
uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq [x] = [x]
uniq (x:y:xs) =
  if x == y
    then uniq (x : xs)
    else x : uniq xs

colorFromRGB :: C.RGB Float -> Color
colorFromRGB c =
  makeColor (C.channelRed c) (C.channelGreen c) (C.channelBlue c) 1

-- Game logic
type GridDim = (Int, Int, Int, Int) -- (xmin, xmax, ymin, ymax)

type Cell = (Int, Int)

data World =
  World
    { alive :: S.Set Cell
    , adjacent :: Cell -> [Cell]
    , dimensions :: GridDim
    }

type WorldS = StateT World IO

potentialCells :: WorldS [Cell]
potentialCells =
  gets adjacent >>= \adj ->
    uniq . sort . concatMap adj . S.toList <$> gets alive

neighbours :: WorldS (Cell -> Int)
neighbours =
  gets alive >>= \al -> ((length . filter (`S.member` al)) .) <$> gets adjacent

xx :: Monad m => m (b -> c) -> m (a -> b) -> m (a -> c)
xx f g = f >>= \f -> (f .) <$> g

adjacentRec :: Int -> WorldS (Cell -> S.Set Cell)
adjacentRec 0 = return S.singleton
adjacentRec n =
  gets adjacent >>= \adj ->
    ((S.fromList . concatMap adj . S.toList) .) <$> adjacentRec (n - 1)

neighboursRec :: Int -> WorldS (Cell -> Int)
neighboursRec n =
  gets alive >>= \al ->
    ((S.size . S.filter (`S.member` al)) .) <$> adjacentRec n

isAliveNext :: WorldS (Cell -> Bool)
isAliveNext =
  gets alive >>= \al ->
    neighbours >>= \nbrs ->
      return (\c -> nbrs c == 3 || (nbrs c == 2 && c `S.member` al))

withAlive :: S.Set Cell -> WorldS ()
withAlive alive = modify (\w -> w {alive = alive})

nextWorld :: WorldS ()
nextWorld =
  isAliveNext >>= \f -> (S.fromList . filter f <$> potentialCells) >>= withAlive

-- Adjacency functions
adjacentEuclidean :: GridDim -> Cell -> [Cell]
adjacentEuclidean _ (x, y) =
  [ (x - 1, y - 1)
  , (x - 1, y)
  , (x - 1, y + 1)
  , (x, y - 1)
  , (x, y + 1)
  , (x + 1, y - 1)
  , (x + 1, y)
  , (x + 1, y + 1)
  ]

adjacentTorus :: GridDim -> Cell -> [Cell]
adjacentTorus (xmin, xmax, ymin, ymax) (x, y) =
  wrap <$>
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

-- Tnitial state builders
stateFromList :: GridDim -> (GridDim -> Cell -> [Cell]) -> [(Int, Int)] -> World
stateFromList d adj l =
  World {alive = S.fromList l, adjacent = adj d, dimensions = d}

centralize :: [(Int, Int)] -> (Int, Int, [(Int, Int)])
centralize l =
  ( (xmax - xmin + 1) `div` 2
  , (ymax - ymin + 1) `div` 2
  , map (\(x, y) -> (x - xc, y - yc)) l)
  where
    xmin = minimum . map fst $ l
    xmax = maximum . map fst $ l
    ymin = minimum . map snd $ l
    ymax = maximum . map snd $ l
    xc = (xmin + xmax) `div` 2
    yc = (ymin + ymax) `div` 2

stateFromListAuto :: [(Int, Int)] -> World
stateFromListAuto l = stateFromList (-d, d, -d, d) adjacentEuclidean l2
  where
    (dx, dy, l2) = centralize l
    d = 4 * max dx dy

-- Read .cells files from stdin
parseCells :: IO World
parseCells =
  stateFromListAuto .
  map snd .
  filter fst .
  concat .
  zipWith (\y -> zipWith (\x c -> (c == 'O', (x, y))) [0 ..]) [0,-1 ..] .
  dropWhile ((== Just '!') . listToMaybe) . lines <$>
  getContents

-- Random initial state
randomWorld :: GridDim -> (GridDim -> Cell -> [Cell]) -> Float -> IO World
randomWorld d@(xmin, xmax, ymin, ymax) adj p =
  stateFromList d adj . catMaybes <$>
  mapM
    (\x -> do
       r1 <- randomRIO (0.0, 1.0) :: IO Float
       return $
         if r1 < p
           then Just x
           else Nothing)
    (liftM2 (,) [xmin .. xmax] [ymin .. ymax])

-- Animation
cellDim :: Float
cellDim = 16

grid :: GridDim -> Picture
grid (xmin, xmax, ymin, ymax) = color (greyN 0.4) . pictures $ vLines ++ hLines
  where
    vLines =
      (\x ->
         line
           ((\y -> ((x - 0.5) * cellDim, (fromIntegral y - 0.5) * cellDim)) <$>
            [ymin, ymax + 1])) .
      fromIntegral <$>
      [xmin .. xmax + 1]
    hLines =
      (\y ->
         line
           ((\x -> ((fromIntegral x - 0.5) * cellDim, (y - 0.5) * cellDim)) <$>
            [xmin, xmax + 1])) .
      fromIntegral <$>
      [ymin .. ymax + 1]

isVisible :: WorldS (Cell -> Bool)
isVisible =
  gets dimensions >>= \(xmin, xmax, ymin, ymax) ->
    return (\(x, y) -> xmin <= x && x <= xmax && ymin <= y && y <= ymax)

cellColor :: WorldS (Cell -> Color)
cellColor =
  neighboursRec 3 >>= \nbrs ->
    return (\c -> colorFromRGB (C.hsl (fromIntegral (nbrs c - 1) * 15) 0.8 0.6))

cellToPicture :: WorldS (Cell -> Maybe Picture)
cellToPicture =
  isVisible >>= \vis ->
    cellColor >>= \col ->
      return $ \c@(x, y) ->
        if vis c
          then Just
                 (color (col c) .
                  translate
                    (cellDim * fromIntegral x)
                    (cellDim * fromIntegral y) $
                  rectangleSolid cellDim cellDim)
          else Nothing

drawWorld :: WorldS Picture
drawWorld =
  cellToPicture >>= \pic ->
    gets dimensions >>= \d ->
      gets (pictures . (grid d :) . mapMaybe pic . S.toList . alive)

main :: IO ()
main = do
  s <- parseCells
  simulateIO
    (InWindow "Conway's Game of Life" (300, 300) (0, 0))
    (greyN 0.1)
    15
    s
    (evalStateT drawWorld)
    (\_ _ -> execStateT nextWorld)
