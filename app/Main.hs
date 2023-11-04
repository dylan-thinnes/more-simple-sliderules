{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Diagrams.Prelude
import Diagrams.TwoD.Image
import Diagrams.Backend.SVG.CmdLine
import Data.Foldable (fold)
import Data.Monoid (Endo(..))
import Text.Printf (printf)

main :: IO ()
main = mainWith myDiagram

myDiagram :: Diagram B
myDiagram =
  let render = renderTick RenderOptions { roFontSize = 14, roYScale = 0.03 }
  in
  frame 0.02 $ vsep 0.02 [foldMap render dScale, foldMap render cScale]

inRange :: Ord a => a -> a -> a -> Bool
inRange lower upper x = lower <= x && x <= upper

cScale :: [Tick]
cScale = map (mapPosition (logBase 10)) $ concat
  [ [Tick 0.1 0.7 pi (Just "π")]
  , forI (divide True 9 (Range 1 10)) (\x -> [Tick 1 0 x (Just (show (round x)))]) $
      \i range -> case i of
        (inRange 0 0 -> True) ->
            for (divide False 10 range) (\x -> [Tick 0.75 0 x (Just (show (round ((x - 1) * 10))))]) $ \range ->
              for (divide False 2 range) (\x -> [Tick 0.5 0 x Nothing]) $ \range ->
                for (divide False 5 range) (\x -> [Tick 0.35 0 x Nothing]) mempty
        (inRange 1 4 -> True) ->
          for (divide False 2 range) (\x -> [Tick 0.75 0 x (Just (show x))]) $ \range ->
            for (divide False 5 range) (\x -> [Tick 0.5 0 x Nothing]) $ \range ->
              for (divide False 4 range) (\x -> [Tick 0.35 0 x Nothing]) mempty
        _ ->
          for (divide False 2 range) (\x -> [Tick 0.75 0 x (Just (show x))]) $ \range ->
            for (divide False 5 range) (\x -> [Tick 0.5 0 x Nothing]) mempty
  ]

dScale :: [Tick]
dScale = map (mapPosition (logBase 100)) $
  for (toRanges True [1, 10, 100]) (\x -> [Tick 1 0 x (Just (show (round x)))]) $ \range -> fold
    [ [Tick 0.1 0.7 (pi * rStart range) (Just "π")]
    , forI (divide False 9 range) (\x -> [Tick 1 0 x (Just (show (round x)))]) $
        \i range -> case i of
          (inRange 0 0 -> True) ->
              for (divide False 10 range) (\x -> [Tick 0.75 0 x (Just (tail (showClean x)))]) $ \range ->
                for (divide False 4 range) (\x -> [Tick 0.5 0 x Nothing]) mempty
          (inRange 1 4 -> True) ->
            for (divide False 2 range) (\x -> [Tick 0.75 0 x (Just (show x))]) $ \range ->
              for (divide False 5 range) (\x -> [Tick 0.5 0 x Nothing]) $ \range ->
                for (divide False 2 range) (\x -> [Tick 0.35 0 x Nothing]) mempty
          _ ->
            for (divide False 2 range) (\x -> [Tick 0.75 0 x (Just (show x))]) $ \range ->
              for (divide False 5 range) (\x -> [Tick 0.5 0 x Nothing]) mempty
    ]

showClean :: Double -> String
showClean = reverse . dropWhile (== '.') . dropWhile (== '0') . reverse . printf "%.6f"

data Tick = Tick
  { _height :: Double
  , _offset :: Double
  , _position :: Double
  , _label :: Maybe String
  }

mapPosition :: (Double -> Double) -> Tick -> Tick
mapPosition f Tick{..} = Tick{_position = f _position, ..}

renderTick :: RenderOptions -> Tick -> Diagram B
renderTick RenderOptions{..} (scaleTickY roYScale -> Tick{..}) =
  moveTo (mkP2 _position _offset) $ fold
    [ moveTo (mkP2 0 0) (scale _height (lw 0.4 (fromOffsets [unitY])))
    , moveTo (mkP2 0 _height) (foldMap (\l -> alignedText 0.5 0 l & fontSize 14) _label)
    ]

scaleTickY :: Double -> Tick -> Tick
scaleTickY factor Tick{..} = Tick{_height = _height * factor, _offset = _offset * factor, ..}

data RenderOptions = RenderOptions
  { roFontSize :: Double
  , roYScale :: Double
  }

data Range = Range { rStart, rEnd :: Double }
  deriving (Show)

divide :: Bool -> Int -> Range -> ([Double], [Range])
divide inclusive amount range = toRanges inclusive $ divideN amount range

toRanges :: Bool -> [Double] -> ([Double], [Range])
toRanges inclusive positions =
  let ranges = zipWith Range positions (tail positions)
      outputPositions = if inclusive then positions else tail (init positions)
  in
  (outputPositions, ranges)

divideN :: Int -> Range -> [Double]
divideN amount Range{..} =
  map (\i -> fromIntegral i * (rEnd - rStart) / fromIntegral amount + rStart) [0..amount]

forI :: Monoid m => ([a], [b]) -> (a -> m) -> (Int -> b -> m) -> m
forI (as, bs) f g = foldMap f as <> fold (zipWith g [0..] bs)

for :: Monoid m => ([a], [b]) -> (a -> m) -> (b -> m) -> m
for (as, bs) f g = foldMap f as <> foldMap g bs
