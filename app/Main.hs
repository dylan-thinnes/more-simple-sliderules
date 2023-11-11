{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PackageImports #-}
module Main where

import Diagrams.Prelude
import Diagrams.TwoD.Image
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine
import Data.Foldable (fold)
import Data.Monoid (Endo(..))
import Text.Printf (printf)
import Data.Maybe (fromJust)
import System.IO (stdin)
import Hershey qualified
import GHC.Exts (IsString(..))

main :: IO ()
main = do
  --mainWith myDiagram
  --mainWith giantSlideRule
  mainWith . myHersheyDiagram =<< fmap fromJust (Hershey.parseFromFile stdin)
  putStrLn "Done!"

ignore :: Monoid b => a -> b
ignore = const mempty

myDiagram :: Diagram B
myDiagram =
  frame 0.04 $
    hsep 0.04
      [ let options = RenderOptions { roRenderLabel = renderLabelText 14, roYScale = 0.015 }
            circles =
              fold
                [ svgId "cScaleCircle" $ foldMap (renderTick options) cScaleCircle
                , rotateBy (negate (logBase 100 pi)) (foldMap (renderTick options) dScaleCircle)
                , foldMap (renderTick options) aScaleCircle
                , foldMap (renderTick options) sqrtScaleSpiral
                ]
            cursor :: Colour Double -> Double -> Diagram B
            cursor color x = fromOffsets [envelopeV (rotateBy (negate x) unitY) circles] & lw 0.4 & lc color
        in
        cursor red 0 <> cursor blue (logBase 100 pi) <> cursor green (logBase 100 pi + logBase 10 2) <> circles
      , let options = RenderOptions { roRenderLabel = renderLabelText 8, roYScale = 0.04 }
        in
        vsep 0.02
          [ foldMap (renderTick options) aScale
          , foldMap (renderTick options) cScale
            ===
            foldMap (renderTick options) dScale
          , foldMap (renderTick options) cfScale
          ]
      ]

myHersheyDiagram :: [Hershey.Character] -> Diagram B
myHersheyDiagram hersheyMap =
  fold
    [ renderWithClass "cScale" hersheyCScaleCircle
    , renderWithClass "dScale" hersheyDScaleCircle
    , renderWithClass "lScale" hersheyLogScaleCircle
    , renderWithClass "aScale" hersheyAScaleCircle
    , renderWithClass "kScale" hersheyKScaleCircle
    , svgId "origin" $ circle 0.001
    ]
  where
  renderWithClass :: String -> [Tick] -> Diagram B
  renderWithClass class_ ticks =
    svgClass class_ $
    foldMap
      (renderTick
        RenderOptions
          { roRenderLabel = renderLabelHershey hersheyMap Hershey.gothicSimplex 0.0004
          , roYScale = 0.015 })
      ticks

  hersheyCScaleCircle :: [Tick]
  hersheyCScaleCircle = map (fmap (TSRadial 0.3 0 . logBase 10)) $ concat
    [ [Tick 0.1 0.7 pi (Just "π") False]
    , forI (divide iStart 9 (Range 1 10)) (\x -> [Tick 1 0 x (Just (tickLabel (show (round x))) { tlFontScaling = 1 }) False]) $
        \i range -> case i of
          (inRange 0 0 -> True) ->
              for (divide iNone 10 range) (\x -> [Tick 0.75 0 x (Just (tickLabel (show (round ((x - 1) * 10)))) { tlFontScaling = 0.8 }) False]) $ \range ->
                for (divide iNone 2 range) (\x -> [Tick 0.5 0 x Nothing False]) $ \range ->
                  for (divide iNone 5 range) (\x -> [Tick 0.35 0 x Nothing False]) mempty
          (inRange 1 4 -> True) ->
            for (divide iNone 2 range) (\x -> [Tick 0.75 0 x (Just (tickLabel (show x)) { tlFontScaling = 0.8 }) False]) $ \range ->
              for (divide iNone 5 range) (\x -> [Tick 0.5 0 x Nothing False]) $ \range ->
                for (divide iNone 4 range) (\x -> [Tick 0.35 0 x Nothing False]) mempty
          _ ->
            for (divide iNone 2 range) (\x -> [Tick 0.75 0 x (Just (tickLabel (show x)) { tlFontScaling = 0.8 }) False]) $ \range ->
              for (divide iNone 5 range) (\x -> [Tick 0.5 0 x Nothing False]) mempty
    ]

  hersheyDScaleCircle :: [Tick]
  hersheyDScaleCircle = map (\t -> t { _pointDown = True }) hersheyCScaleCircle

  hersheyLogScaleCircle :: [Tick]
  hersheyLogScaleCircle = map (fmap (TSRadial 0.24 0)) $ concat
    [ for (divide iStart 10 (Range 0 1)) (\x -> [Tick 1 0 x (Just (tickLabel (show (round (10 * x)))) { tlFontScaling = 1 }) False]) $ \range ->
        for (divide iNone 2 range) (\x -> [Tick 0.75 0 x Nothing False]) $ \range ->
          for (divide iNone 5 range) (\x -> [Tick 0.5 0 x Nothing False]) $ \range ->
            for (divide iNone 4 range) (\x -> [Tick 0.3 0 x Nothing False]) $ \range ->
              mempty
    ]

  hersheyAScaleCircle :: [Tick]
  hersheyAScaleCircle = map (fmap (TSRadial 0.34 0 . logBase 100)) $
    forI (toRanges iStart [1, 10, 100]) (\x -> [Tick 1 0 x (Just (tickLabel (show (round x)))) False]) $ \i range ->
      let showLargerTick = show . round
          showSmallerTick1To2
            | i > 0 = show . round
            | otherwise = show
          showSmallerTick3Up
            | i > 0 = show . round
            | otherwise = \x -> show (round ((x - 1) * 10))
      in
      forI (divide iNone 9 range) (\x -> [Tick 1 0 x (Just (tickLabel (showLargerTick x)) { tlFontScaling = 1 }) False]) $
        \i range -> case i of
          (inRange 0 0 -> True) ->
              for (divide iNone 10 range) (\x -> [Tick 0.75 0 x (Just (tickLabel (showSmallerTick1To2 x)) { tlFontScaling = 0.8 }) False]) $ \range ->
                for (divide iNone 2 range) (\x -> [Tick 0.5 0 x Nothing False]) $ \range ->
                  for (divide iNone 5 range) (\x -> [Tick 0.35 0 x Nothing False]) mempty
          (inRange 1 4 -> True) ->
            for (divide iNone 2 range) (\x -> [Tick 0.75 0 x (Just (tickLabel (showSmallerTick3Up x)) { tlFontScaling = 0.8 }) False]) $ \range ->
              for (divide iNone 5 range) (\x -> [Tick 0.5 0 x Nothing False]) $ \range ->
                for (divide iNone 4 range) (\x -> [Tick 0.35 0 x Nothing False]) mempty
          _ ->
            for (divide iNone 2 range) (\x -> [Tick 0.75 0 x (Just (tickLabel (showSmallerTick3Up x)) { tlFontScaling = 0.8 }) False]) $ \range ->
              for (divide iNone 5 range) (\x -> [Tick 0.5 0 x Nothing False]) mempty

  hersheyKScaleCircle :: [Tick]
  hersheyKScaleCircle = map (fmap (TSRadial 0.38 0 . logBase 1000)) $
    forI (toRanges iStart [1, 10, 100, 1000]) (\x -> [Tick 1 0 x (Just (tickLabel (show (round x)))) False]) $ \i range ->
      let showLargerTick = Just . show . round
          showSmallerTick1To2
            | i > 0 = Just . show . round
            | otherwise = Just . show
          showSmallerTick3To6
            | i > 0 = Just . show . round
            | otherwise = \x -> Just (show (round ((x - 1) * 10)))
          showSmallerTick7Up
            | i > 0 = const Nothing
            | otherwise = \x -> Just (show (round ((x - 1) * 10)))
          tickLabelWithFontScaling scale text = (tickLabel text) { tlFontScaling = scale }
      in
      forI (divide iNone 9 range) (\x -> [Tick 1 0 x (tickLabelWithFontScaling 1 <$> showLargerTick x) False]) $
        \i range -> case i of
          (inRange 0 1 -> True) ->
              for (divide iNone 2 range) (\x -> [Tick 0.75 0 x (tickLabelWithFontScaling 0.8 <$> showSmallerTick1To2 x) False]) $ \range ->
                for (divide iNone 5 range) (\x -> [Tick 0.5 0 x Nothing False]) $ \range ->
                  for (divide iNone 4 range) (\x -> [Tick 0.35 0 x Nothing False]) mempty
          (inRange 2 4 -> True) ->
            for (divide iNone 2 range) (\x -> [Tick 0.75 0 x (tickLabelWithFontScaling 0.8 <$> showSmallerTick3To6 x) False]) $ \range ->
              for (divide iNone 5 range) (\x -> [Tick 0.5 0 x Nothing False]) $ \range ->
                mempty
          _ ->
            for (divide iNone 2 range) (\x -> [Tick 0.75 0 x (tickLabelWithFontScaling 0.8 <$> showSmallerTick7Up x) False]) $ \range ->
              for (divide iNone 5 range) (\x -> [Tick 0.5 0 x Nothing False]) mempty

giantSlideRule :: Diagram B
giantSlideRule = foldMap (renderTick RenderOptions { roRenderLabel = renderLabelText 14, roYScale = 0.015 }) longCScale
  where
    longCScale =
      map (fmap (TSRadial 0.4 0.05 . (* 10) . logBase 10)) $ fold
        [ [Tick 0.1 0.7 pi (Just "π") False]
        , forI (divide iBoth 9 (Range 1 10)) (\x -> [Tick 1 0 x (Just (tickLabel (showClean x))) False]) $
            \i range -> case i of
              (inRange 0 1 -> True) ->
                for (divide iNone 10 range) (\x -> [Tick 1 0 x (Just (tickLabel (showClean x))) False]) $ \range ->
                  for (divide iNone 2 range) (\x -> [Tick 0.75 0 x Nothing False]) $ \range ->
                    for (divide iNone 5 range) (\x -> [Tick 0.5 0 x Nothing False]) $ \range ->
                      for (divide iNone 2 range) (\x -> [Tick 0.35 0 x Nothing False]) $ \range ->
                        for (divide iNone 5 range) (\x -> [Tick 0.25 0 x Nothing False]) mempty
              _ ->
                for (divide iNone 10 range) (\x -> [Tick 1 0 x (Just (tickLabel (showClean x))) False]) $ \range ->
                  for (divide iNone 2 range) (\x -> [Tick 0.75 0 x Nothing False]) $ \range ->
                    for (divide iNone 5 range) (\x -> [Tick 0.5 0 x Nothing False]) $ \range ->
                      for (divide iNone 2 range) (\x -> [Tick 0.35 0 x Nothing False]) mempty
        ]


inRange :: Ord a => a -> a -> a -> Bool
inRange lower upper x = lower <= x && x <= upper

cScalePositions :: Inclusive -> [TickG Double]
cScalePositions inclusive = concat
  [ [Tick 0.1 0.7 pi (Just "π") False]
  , forI (divide inclusive 9 (Range 1 10)) (\x -> [Tick 1 0 x (Just (tickLabel (show (round x)))) False]) $
      \i range -> case i of
        (inRange 0 0 -> True) ->
            for (divide iNone 10 range) (\x -> [Tick 0.75 0 x (Just (tickLabel (show (round ((x - 1) * 10))))) False]) $ \range ->
              for (divide iNone 2 range) (\x -> [Tick 0.5 0 x Nothing False]) $ \range ->
                for (divide iNone 5 range) (\x -> [Tick 0.35 0 x Nothing False]) mempty
        (inRange 1 4 -> True) ->
          for (divide iNone 2 range) (\x -> [Tick 0.75 0 x (Just (tickLabel (show x))) False]) $ \range ->
            for (divide iNone 5 range) (\x -> [Tick 0.5 0 x Nothing False]) $ \range ->
              for (divide iNone 4 range) (\x -> [Tick 0.35 0 x Nothing False]) mempty
        _ ->
          for (divide iNone 2 range) (\x -> [Tick 0.75 0 x (Just (tickLabel (show x))) False]) $ \range ->
            for (divide iNone 5 range) (\x -> [Tick 0.5 0 x Nothing False]) mempty
  ]

cScale, dScale :: [Tick]
cScale = map (fmap (TSLinear . logBase 10)) (cScalePositions iBoth)
dScale = map (\t -> t { _pointDown = True }) cScale

aScalePositions :: Inclusive -> [TickG Double]
aScalePositions inclusive =
  for (toRanges inclusive [1, 10, 100]) (\x -> [Tick 1 0 x (Just (tickLabel (show (round x)))) False]) $ \range -> fold
    [ [Tick 0.1 0.7 (pi * rStart range) (Just "π") False]
    , forI (divide iNone 9 range) (\x -> [Tick 1 0 x (Just (tickLabel (show (round x)))) False]) $
        \i range -> case i of
          (inRange 0 0 -> True) ->
              for (divide iNone 10 range) (\x -> [Tick 0.75 0 x (Just (tickLabel (tail (showClean x)))) False]) $ \range ->
                for (divide iNone 4 range) (\x -> [Tick 0.5 0 x Nothing False]) mempty
          (inRange 1 4 -> True) ->
            for (divide iNone 2 range) (\x -> [Tick 0.75 0 x (Just (tickLabel (showClean x))) False]) $ \range ->
              for (divide iNone 5 range) (\x -> [Tick 0.5 0 x Nothing False]) $ \range ->
                for (divide iNone 2 range) (\x -> [Tick 0.35 0 x Nothing False]) mempty
          _ ->
            for (divide iNone 2 range) (\x -> [Tick 0.75 0 x (Just (tickLabel (showClean x))) False]) $ \range ->
              for (divide iNone 5 range) (\x -> [Tick 0.5 0 x Nothing False]) mempty
    ]

llScale :: [Tick]
llScale = map (fmap (TSLinear . (3 +) . logBase 10 . logBase 10)) $ concat $
  let controlPoints = [1.0025, 1.002, 1.003, 1.004, 1.005, 1.006, 1.007, 1.008, 1.009, 1.01, 1.015, 1.02, 1.03]
  in
  [ for (toRanges iBoth controlPoints) (\x -> [Tick 1 0 x (Just (tickLabel (show x))) False]) $ \range ->
      for (autodivide iNone range) (\x -> [Tick 1 0 x Nothing False]) $ \range ->
        mempty
    --forI (divide inclusive 9 (Range 1 10)) (\x -> [Tick 1 0 x (Just (tickLabel (show (round x)))) False]) $
    --  \i range -> case i of
    --    (inRange 0 0 -> True) ->
    --        for (divide iNone 10 range) (\x -> [Tick 0.75 0 x (Just (tickLabel (show (round ((x - 1) * 10))))) False]) $ \range ->
    --          for (divide iNone 2 range) (\x -> [Tick 0.5 0 x Nothing False]) $ \range ->
    --            for (divide iNone 5 range) (\x -> [Tick 0.35 0 x Nothing False]) mempty
    --    (inRange 1 4 -> True) ->
    --      for (divide iNone 2 range) (\x -> [Tick 0.75 0 x (Just (tickLabel (show x))) False]) $ \range ->
    --        for (divide iNone 5 range) (\x -> [Tick 0.5 0 x Nothing False]) $ \range ->
    --          for (divide iNone 4 range) (\x -> [Tick 0.35 0 x Nothing False]) mempty
    --    _ ->
    --      for (divide iNone 2 range) (\x -> [Tick 0.75 0 x (Just (tickLabel (show x))) False]) $ \range ->
    --        for (divide iNone 5 range) (\x -> [Tick 0.5 0 x Nothing False]) mempty
  ]

aScale :: [Tick]
aScale = map (fmap (TSLinear . logBase 100)) (aScalePositions iBoth)

cfScale :: [Tick]
cfScale = do
  tick <- cScalePositions iStart
  flip traverse tick $ \pos -> do
    transformedPos <- clamp 0.023 0.024 $ logBase 10 $ pos / pi
    pure (TSLinear transformedPos)
  where
    clamp :: Double -> Double -> Double -> [Double]
    clamp lower upper x
      | x < -lower = clamp lower upper (x + 1)
      | x < upper = [x, x + 1]
      | x < 1 - lower = [x]
      | x < 1 + upper = [x, x - 1]
      | otherwise = clamp lower upper (x - 1)

cScaleCircle, dScaleCircle :: [Tick]
cScaleCircle = map (fmap (TSRadial 0.3 0 . logBase 10)) (cScalePositions iStart)
dScaleCircle = map (\t -> t { _pointDown = True }) cScaleCircle

aScaleCircle :: [Tick]
aScaleCircle = map (fmap (TSRadial 0.35 0 . logBase 100)) (aScalePositions iStart)

sqrtScaleSpiral :: [Tick]
sqrtScaleSpiral = map (fmap (TSRadial 0.4 0.05 . (* 2) . logBase 10)) $ fold
  [ [Tick 0.1 0.7 pi (Just "π") False]
  , forI (divide iBoth 9 (Range 1 10)) (\x -> [Tick 1 0 x (Just (tickLabel (showClean x))) False]) $
      \i range -> case i of
        (inRange 0 0 -> True) ->
          for (divide iNone 10 range) (\x -> [Tick 1 0 x (Just (tickLabel (showClean x))) False]) $ \range ->
            for (divide iNone 2 range) (\x -> [Tick 0.75 0 x Nothing False]) $ \range ->
              for (divide iNone 5 range) (\x -> [Tick 0.5 0 x Nothing False]) $ \range ->
                for (divide iNone 2 range) (\x -> [Tick 0.35 0 x Nothing False]) mempty
        (inRange 1 4 -> True) ->
          for (divide iNone 10 range) (\x -> [Tick 1 0 x (Just (tickLabel (showClean x))) False]) $ \range ->
            for (divide iNone 2 range) (\x -> [Tick 0.75 0 x Nothing False]) $ \range ->
              for (divide iNone 5 range) (\x -> [Tick 0.5 0 x Nothing False]) mempty
        _ ->
          for (divide iNone 2 range) (\x -> [Tick 1 0 x (Just (tickLabel (showClean x))) False]) $ \range ->
            for (divide iNone 5 range) (\x -> [Tick 1 0 x Nothing False]) $ \range ->
              for (divide iNone 2 range) (\x -> [Tick 0.75 0 x Nothing False]) $ \range ->
                for (divide iNone 2 range) (\x -> [Tick 0.5 0 x Nothing False]) mempty
  ]

showClean :: Double -> String
showClean = reverse . dropWhile (== '.') . dropWhile (== '0') . reverse . printf "%.6f"

data TickG position = Tick
  { _height :: Double
  , _offset :: Double
  , _position :: position
  , _label :: Maybe TickLabel
  , _pointDown :: Bool
  }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

type Tick = TickG TickShape

data TickShape = TSRadial { tsRadius, tsSpiralRate, tsAngle :: Double } | TSLinear Double
  deriving (Show, Eq, Ord)

data TickLabel = TickLabel
  { tlFontScaling :: Double
  , tlText :: String
  }
  deriving (Show, Eq, Ord)

tickLabel :: String -> TickLabel
tickLabel = TickLabel 1

instance IsString TickLabel where
  fromString = tickLabel

renderTick :: RenderOptions -> Tick -> Diagram B
renderTick RenderOptions{..} (scaleTickY roYScale -> tick@Tick{..}) =
  moveByPositionOffset $ fold
    [ moveTo (mkP2 0 0) (scale (flipIfDown _height) (lw 0.4 (fromOffsets [unitY])))
    , moveTo (mkP2 0 (flipIfDown _height)) (foldMap (roRenderLabel tick) _label)
    ]
      where
    flipIfDown = if _pointDown then negate else id

    moveByPositionOffset = case _position of
      TSLinear position -> moveTo (mkP2 position (flipIfDown _offset))
      TSRadial radius spiralRate angle -> rotateBy (-angle) . moveTo (mkP2 0 ((angle * spiralRate) + radius + flipIfDown _offset))

scaleTickY :: Double -> Tick -> Tick
scaleTickY factor Tick{..} = Tick{_height = _height * factor, _offset = _offset * factor, ..}

data RenderOptionsG a = RenderOptions
  { roRenderLabel :: TickG a -> TickLabel -> Diagram B
  , roYScale :: Double
  }

type RenderOptions = RenderOptionsG TickShape

renderLabelText :: Double -> TickG a -> TickLabel -> Diagram B
renderLabelText roFontSize Tick{..} TickLabel{..}
  | _pointDown = alignedText 0.5 1 tlText & fontSize (pure (roFontSize * tlFontScaling))
  | otherwise  = alignedText 0.5 0 tlText & fontSize (pure (roFontSize * tlFontScaling))

renderLabelHershey :: [Hershey.Character] -> [Int] -> Double -> TickG a -> TickLabel -> Diagram B
renderLabelHershey chars idxs scaleFactor Tick{..} TickLabel{..}
  = Hershey.renderWrite chars idxs Hershey.TextOptions { justify = 0, aboveBaseline = 15, belowBaseline = 15 } [tlText]
      & scale (scaleFactor * tlFontScaling)
      & if _pointDown then alignT else alignB

data Range = Range { rStart, rEnd :: Double }
  deriving (Show)

data Inclusive = Inclusive Bool Bool

iBoth, iStart, iEnd, iNone :: Inclusive
iBoth  = Inclusive  True  True
iStart = Inclusive  True False
iEnd   = Inclusive False  True
iNone  = Inclusive False False

autodivide :: Inclusive -> Range -> ([Double], [Range])
autodivide inclusive range@Range{..} = divide inclusive amount range
  where
    amount = scaleUntilIntegral (abs (rEnd - rStart))
    scaleUntilIntegral x =
      if abs (x - fromIntegral (round x)) < 0.0001
         then round x
         else scaleUntilIntegral (x * 10)

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
