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
  gets (\w -> uniq . sort . concatMap (adjacent w) . S.toList . alive $ w)

neighbours :: WorldS (Cell -> Int)
neighbours = gets (\w -> length . filter (`S.member` alive w) . adjacent w)

-- NOTE: Might need to change to Int -> WorldS (Cell -> S.Set Cell)
adjacentRec :: Int -> WorldS (Cell -> S.Set Cell)
adjacentRec 0 = gets (const S.singleton)
adjacentRec n =
  adjacentRec (n - 1) >>=
  (\a -> gets (\w -> S.fromList . concatMap (adjacent w) . S.toList . a))

neighboursRec :: Int -> WorldS (Cell -> Int)
neighboursRec n =
  adjacentRec n >>=
  (\a -> gets (\w -> S.size . S.filter (`S.member` alive w) . a))

isAliveNext :: WorldS (Cell -> Bool)
isAliveNext =
  neighbours >>=
  (\nbrs -> gets (\w c -> nbrs c == 3 || (nbrs c == 2 && c `S.member` alive w)))

nextWorld :: WorldS ()
nextWorld =
  potentialCells >>=
  (\p ->
     isAliveNext >>=
     (\f -> modify (\w -> w {alive = S.fromList . filter f $ p})))

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
parseCells = do
  lns <- dropWhile ((== Just '!') . listToMaybe) . lines <$> getContents
  let t =
        concat $
        zipWith
          (\y ->
             catMaybes .
             zipWith
               (\x c ->
                  if c == 'O'
                    then Just (x, y)
                    else Nothing)
               [0 ..])
          [0,-1 ..]
          lns
  return $ stateFromListAuto t

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

isVisible :: WorldS (Cell -> Bool)
isVisible =
  gets
    (\w (x, y) ->
       let (xmin, xmax, ymin, ymax) = dimensions w
        in xmin <= x && x <= xmax && ymin <= y && y <= ymax)

cellColor :: WorldS (Cell -> Color)
cellColor =
  neighboursRec 3 >>=
  (\nbrs ->
     gets
       (\w c -> colorFromRGB (C.hsl (fromIntegral (nbrs c - 1) * 15) 0.8 0.6)))

cellToPicture :: WorldS (Cell -> Maybe Picture)
cellToPicture =
  isVisible >>=
  (\vis ->
     cellColor >>=
     (\col ->
        gets
          (\w c@(x, y) ->
             if vis c
               then Just
                      (color (col c) .
                       translate
                         (cellDim * fromIntegral x)
                         (cellDim * fromIntegral y) $
                       rectangleSolid cellDim cellDim)
               else Nothing)))

drawWorld :: WorldS Picture
drawWorld =
  cellToPicture >>=
  (\pic ->
     gets
       (\w ->
          pictures . (grid (dimensions w) :) . mapMaybe pic . S.toList . alive $
          w))

main :: IO ()
main
 = do
  s <- parseCells
  simulateIO
    (InWindow "Conway's Game of Life" (300, 300) (0, 0))
    (greyN 0.1)
    15
    s
    (evalStateT drawWorld)
    (\_ _ -> execStateT nextWorld)
