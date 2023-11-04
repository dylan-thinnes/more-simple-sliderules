{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
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
main = do
  mainWith myDiagram
  putStrLn "Done!"

myDiagram :: Diagram B
myDiagram =
  frame 0.04 $
    hsep 0.04
      [ foldMap (renderTick RenderOptions { roFontSize = 14, roYScale = 0.015 }) cScaleCircle
      , vsep 0.02
          [ foldMap (renderTick RenderOptions { roFontSize = 8, roYScale = 0.04 }) dScale
          , foldMap (renderTick RenderOptions { roFontSize = 8, roYScale = 0.04 }) cScale
          ]
      ]

inRange :: Ord a => a -> a -> a -> Bool
inRange lower upper x = lower <= x && x <= upper

cScale :: [Tick]
cScale = map (fmap (TSLinear . logBase 10)) $ concat
  [ [Tick 0.1 0.7 pi (Just "π")]
  , forI (divide iBoth 9 (Range 1 10)) (\x -> [Tick 1 0 x (Just (show (round x)))]) $
      \i range -> case i of
        (inRange 0 0 -> True) ->
            for (divide iNone 10 range) (\x -> [Tick 0.75 0 x (Just (show (round ((x - 1) * 10))))]) $ \range ->
              for (divide iNone 2 range) (\x -> [Tick 0.5 0 x Nothing]) $ \range ->
                for (divide iNone 5 range) (\x -> [Tick 0.35 0 x Nothing]) mempty
        (inRange 1 4 -> True) ->
          for (divide iNone 2 range) (\x -> [Tick 0.75 0 x (Just (show x))]) $ \range ->
            for (divide iNone 5 range) (\x -> [Tick 0.5 0 x Nothing]) $ \range ->
              for (divide iNone 4 range) (\x -> [Tick 0.35 0 x Nothing]) mempty
        _ ->
          for (divide iNone 2 range) (\x -> [Tick 0.75 0 x (Just (show x))]) $ \range ->
            for (divide iNone 5 range) (\x -> [Tick 0.5 0 x Nothing]) mempty
  ]

dScale :: [Tick]
dScale = map (fmap (TSLinear . logBase 100)) $
  for (toRanges iBoth [1, 10, 100]) (\x -> [Tick 1 0 x (Just (show (round x)))]) $ \range -> fold
    [ [Tick 0.1 0.7 (pi * rStart range) (Just "π")]
    , forI (divide iNone 9 range) (\x -> [Tick 1 0 x (Just (show (round x)))]) $
        \i range -> case i of
          (inRange 0 0 -> True) ->
              for (divide iNone 10 range) (\x -> [Tick 0.75 0 x (Just (tail (showClean x)))]) $ \range ->
                for (divide iNone 4 range) (\x -> [Tick 0.5 0 x Nothing]) mempty
          (inRange 1 4 -> True) ->
            for (divide iNone 2 range) (\x -> [Tick 0.75 0 x (Just (showClean x))]) $ \range ->
              for (divide iNone 5 range) (\x -> [Tick 0.5 0 x Nothing]) $ \range ->
                for (divide iNone 2 range) (\x -> [Tick 0.35 0 x Nothing]) mempty
          _ ->
            for (divide iNone 2 range) (\x -> [Tick 0.75 0 x (Just (showClean x))]) $ \range ->
              for (divide iNone 5 range) (\x -> [Tick 0.5 0 x Nothing]) mempty
    ]

cScaleCircle :: [Tick]
cScaleCircle = map (fmap (TSRadial 0.3 . logBase 10)) $ concat
  [ [Tick 0.1 0.7 pi (Just "π")]
  , forI (divide iStart 9 (Range 1 10)) (\x -> [Tick 1 0 x (Just (show (round x)))]) $
      \i range -> case i of
        (inRange 0 0 -> True) ->
            for (divide iNone 10 range) (\x -> [Tick 0.75 0 x (Just (show (round ((x - 1) * 10))))]) $ \range ->
              for (divide iNone 2 range) (\x -> [Tick 0.5 0 x Nothing]) $ \range ->
                for (divide iNone 5 range) (\x -> [Tick 0.35 0 x Nothing]) mempty
        (inRange 1 4 -> True) ->
          for (divide iNone 2 range) (\x -> [Tick 0.75 0 x (Just (show x))]) $ \range ->
            for (divide iNone 5 range) (\x -> [Tick 0.5 0 x Nothing]) $ \range ->
              for (divide iNone 4 range) (\x -> [Tick 0.35 0 x Nothing]) mempty
        _ ->
          for (divide iNone 2 range) (\x -> [Tick 0.75 0 x (Just (show x))]) $ \range ->
            for (divide iNone 5 range) (\x -> [Tick 0.5 0 x Nothing]) mempty
  ]

showClean :: Double -> String
showClean = reverse . dropWhile (== '.') . dropWhile (== '0') . reverse . printf "%.6f"

data TickG position = Tick
  { _height :: Double
  , _offset :: Double
  , _position :: position
  , _label :: Maybe String
  }
  deriving (Show, Eq, Ord, Functor)

type Tick = TickG TickShape

data TickShape = TSRadial { tsRadius, tsAngle :: Double } | TSLinear Double
  deriving (Show, Eq, Ord)

renderTick :: RenderOptions -> Tick -> Diagram B
renderTick RenderOptions{..} (scaleTickY roYScale -> Tick{..}) =
  moveByPositionOffset $ fold
    [ moveTo (mkP2 0 0) (scale _height (lw 0.4 (fromOffsets [unitY])))
    , moveTo (mkP2 0 _height) (foldMap (\l -> alignedText 0.5 0 l & fontSize (pure roFontSize)) _label)
    ]
      where
    moveByPositionOffset = case _position of
      TSLinear position -> moveTo (mkP2 position _offset)
      TSRadial radius angle -> rotateBy (-angle) . moveTo (mkP2 0 (radius + _offset))

scaleTickY :: Double -> Tick -> Tick
scaleTickY factor Tick{..} = Tick{_height = _height * factor, _offset = _offset * factor, ..}

data RenderOptions = RenderOptions
  { roFontSize :: Double
  , roYScale :: Double
  }

data Range = Range { rStart, rEnd :: Double }
  deriving (Show)

data Inclusive = Inclusive Bool Bool

iBoth, iStart, iEnd, iNone :: Inclusive
iBoth  = Inclusive  True  True
iStart = Inclusive  True False
iEnd   = Inclusive False  True
iNone  = Inclusive False False

divide :: Inclusive -> Int -> Range -> ([Double], [Range])
divide inclusive amount range = toRanges inclusive $ divideN amount range

toRanges :: Inclusive -> [Double] -> ([Double], [Range])
toRanges (Inclusive start end) positions =
  let ranges = zipWith Range positions (tail positions)
      outputPositions =
        (if start then id else tail) $ (if end then id else init) positions
  in
  (outputPositions, ranges)

divideN :: Int -> Range -> [Double]
divideN amount Range{..} =
  map (\i -> fromIntegral i * (rEnd - rStart) / fromIntegral amount + rStart) [0..amount]

forI :: Monoid m => ([a], [b]) -> (a -> m) -> (Int -> b -> m) -> m
forI (as, bs) f g = foldMap f as <> fold (zipWith g [0..] bs)

for :: Monoid m => ([a], [b]) -> (a -> m) -> (b -> m) -> m
for (as, bs) f g = foldMap f as <> foldMap g bs
