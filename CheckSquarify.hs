import Control.Applicative
import Test.QuickCheck
import Squarify

import Debug.Trace

instance Arbitrary Rectangle where
  arbitrary = do
    x <- abs <$> arbitrary
    y <- abs <$> arbitrary
    w <- (\v -> 1 + abs v) <$> arbitrary
    h <- (\v -> 1 + abs v) <$> arbitrary
    return $ Rectangle x y w h

rectArea (Rectangle _ _ w h) = w * h

nearlyEqual a b = 1e-6 > abs (a - b)

notLessThan a b = a - b > -1e-6

contains (Rectangle x1 y1 w1 h1) (Rectangle x2 y2 w2 h2) =
  let (x1', y1') = (x1+w1, y1+h1)
      (x2', y2') = (x2+w2, y2+h2) in
  and [
    x2  `notLessThan` x1,
    y2  `notLessThan` y1,
    x1' `notLessThan` x2',
    y1' `notLessThan` y2'
    ]

disjoint (Rectangle x1 y1 w1 h1) (Rectangle x2 y2 w2 h2) =
  let (x1', y1') = (x1+w1, y1+h1)
      (x2', y2') = (x2+w2, y2+h2) in
  or [
    x2 `notLessThan` x1',
    x1 `notLessThan` x2',
    y2 `notLessThan` y1',
    y1 `notLessThan` y2'
    ]

data RectAndAreas = RectAndAreas { rectangle::Rectangle, areas::[Area] } deriving Show

instance Arbitrary RectAndAreas where
  arbitrary = do
    rect <- arbitrary
    let totalArea = rectArea rect
    areas <- ((map abs) <$> arbitrary) `suchThat` \areas -> totalArea >= sum areas
    return $ RectAndAreas rect areas

checkSplitRectangle1 (RectAndAreas r0 areas) =
  ((rectArea r1) + (rectArea r2)) `nearlyEqual` (rectArea r0) &&
  r0 `contains` r1 &&
  r0 `contains` r2 &&
  r1 `disjoint` r2
  where (r1, r2) = splitRect r0 areas

checkSplitRectangle2 (RectAndAreas rect areas) = (rectArea r1) `nearlyEqual` sum areas
  where (r1, _) = splitRect rect areas

checkSquarify (RectAndAreas rect areas) =
  and [a `nearlyEqual` rectArea r | (a,r) <- zip areas rs] &&

  and [(rs !! i) `disjoint` (rs !! j) |
       let n = length rs,
       i <- [0   .. n-1],
       j <- [i+1 .. n-1]] &&

  and [rect `contains` r | r <- rs]

  where rs = squarify rect areas []
