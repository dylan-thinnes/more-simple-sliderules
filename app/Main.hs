{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Diagrams.Prelude
import Diagrams.TwoD.Image
import Diagrams.Backend.SVG.CmdLine
import Data.Foldable (fold)

main :: IO ()
main = mainWith myDiagram

myDiagram :: Diagram B
myDiagram = frame 0.1 $ foldMap renderTick cScale

inRange :: Ord a => a -> a -> a -> Bool
inRange lower upper x = lower <= x && x <= upper

cScale :: [Tick]
cScale = map (mapPosition log) $
  forI (divide True 9 (Range 1 10)) (\x -> [Tick 0.1 x (Just (show (round x)))]) $
    \i range -> case i of
      (inRange 0 0 -> True) ->
          for (divide False 10 range) (\x -> [Tick 0.075 x (Just (show (round ((x - 1) * 10))))]) $ \range ->
            for (divide False 2 range) (\x -> [Tick 0.05 x Nothing]) $ \range ->
              for (divide False 5 range) (\x -> [Tick 0.035 x Nothing]) mempty
      (inRange 1 4 -> True) ->
        for (divide False 2 range) (\x -> [Tick 0.075 x (Just (show x))]) $ \range ->
          for (divide False 5 range) (\x -> [Tick 0.05 x Nothing]) $ \range ->
            for (divide False 4 range) (\x -> [Tick 0.035 x Nothing]) mempty
      _ ->
        for (divide False 2 range) (\x -> [Tick 0.075 x (Just (show x))]) $ \range ->
          for (divide False 5 range) (\x -> [Tick 0.05 x Nothing]) mempty

data Tick = Tick
  { height :: Double
  , position :: Double
  , label :: Maybe String
  }

t :: [a -> Tick -> Tick] -> a -> Tick
t fs x = foldr (\f t -> f x t) (Tick 0 0 Nothing) fs

mapPosition :: (Double -> Double) -> Tick -> Tick
mapPosition f Tick{..} = Tick{position = f position, ..}

renderTick :: Tick -> Diagram B
renderTick Tick{..} = fold
  [ moveTo (mkP2 position 0) (scale height (lw 0.4 (fromOffsets [unitY])))
  , moveTo (mkP2 position height) (foldMap (\l -> text l & fontSize 14) label)
  ]


data Range = Range Double Double
  deriving (Show)

divide :: Bool -> Int -> Range -> ([Double], [Range])
divide inclusive amount (Range start end) =
  let positions = map (\i -> fromIntegral i * (end - start) / fromIntegral amount + start) [0..amount]
      ranges = zipWith Range positions (tail positions)
      outputPositions = if inclusive then positions else tail (init positions)
  in
  (outputPositions, ranges)

forI :: Monoid m => ([a], [b]) -> (a -> m) -> (Int -> b -> m) -> m
forI (as, bs) f g = foldMap f as <> fold (zipWith g [0..] bs)

for :: Monoid m => ([a], [b]) -> (a -> m) -> (b -> m) -> m
for (as, bs) f g = foldMap f as <> foldMap g bs
