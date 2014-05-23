module Squarify where

import Data.List
import Text.Printf
import Text.XML.Light
import GHC.Exts (sortWith)

import CTM (Tree(..), treeSize, children)

type Length = Double
type Area = Double

data Rectangle = Rectangle { x, y, w, h :: Length } deriving (Show, Eq)

colors = ["White", "Silver", "Gray", "Red", "Maroon", "Yellow", "Olive",
          "Lime", "Green", "Aqua", "Teal", "Blue", "Navy", "Fuchsia", "Purple"]

aspectRatio :: Length -> Area -> Double
aspectRatio l0 area =
  let l1 = area / l0 in
  exp $ abs $ log $ l1 / l0

aspectRatios l0 areas =
  let totalArea = sum areas
      l1 = totalArea / l0 in
  map (aspectRatio l1) areas

worstAspectRatio l0 areas = maximum $ aspectRatios l0 areas

relativeImprovement l0 areas = [war - war' | (war, war') <- zip wars $ tail wars]
  where wars = map (worstAspectRatio l0) $ tail $ inits areas

areasForRow :: Length -> [Area] -> ([Area], [Area])
areasForRow l0 areas = ((head areas):better', worse')
  where withImprovements = zip (tail areas) (relativeImprovement l0 areas)
        (better, worse)  = break (\(a,i) -> i < 0) withImprovements
        better' = fst $ unzip better
        worse' = fst $ unzip worse

squarify rect [] rects = rects
squarify rect areas rects = squarify remainingRect remainingAreas $ rects ++ newRow
  where (useAreas, remainingAreas) = areasForRow (min w h) areas
        (newRect, remainingRect) = splitRect rect useAreas
        newRow = makeRow newRect useAreas
        Rectangle x y w h = rect

splitRect :: Rectangle -> [Area] -> (Rectangle, Rectangle)
splitRect rect areas | h0 > w0   = (rect { h = l1 }, rect { y = y0 + l1, h = h0 - l1 })
                     | otherwise = (rect { w = l1 }, rect { x = x0 + l1, w = w0 - l1 })
  where totalArea = sum areas
        l0 = min w0 h0
        l1 = totalArea / l0
        Rectangle x0 y0 w0 h0 = rect

makeRow rect areas | h0 > w0   = [rect { h = l, y = y' } |
                                  let ls = [h0*a/totalArea | a <- areas],
                                  (l, y') <- zip ls $ scanl (+) y0 ls]

                   | otherwise = [rect { w = l, x = x' } |
                                  let ls = [w0*a/totalArea | a <- areas],
                                  (l, x') <- zip ls $ scanl (+) x0 ls]
  where Rectangle x0 y0 w0 h0 = rect
        totalArea = sum areas

-- makeTreeMap' rect (Tree _ _ []) = [rect]
makeTreeMap' rect tree = rect : (concat $ [makeTreeMap' r t | (r, t) <- zip rs subtrees])
  where rs = squarify rect areas []
        areas = map treeSize subtrees
        subtrees = sortWith (\c -> -1 * treeSize c) $
                   filter (\c -> 0 < treeSize c) $
                   children tree

makeTreeMap tree = makeTreeMap' rect tree
  where rect = Rectangle 0 0 l l
        l = sqrt $ treeSize tree

svgRectangle :: String -> Rectangle -> Element
svgRectangle color (Rectangle x y w h) =
  unode "rect" [Attr (unqual "x") (printf "%.2f" x),
                Attr (unqual "y") (printf "%.2f" y),
                Attr (unqual "width") (printf "%.2f" w),
                Attr (unqual "height") (printf "%.2f" h),
                Attr (unqual "stroke-width") (printf "%.2f" $ (*) 0.03 $ min w h),
                Attr (unqual "stroke") "black",
                Attr (unqual "fill") color
                -- Attr (unqual "fill-opacity") "0.2"
               ]

svgStm :: Double -> Double -> Tree -> Element
svgStm width height tree = unode "svg" ([Attr (unqual "xmlns") "http://www.w3.org/2000/svg",
                                         Attr (unqual "width") (printf "%.0fpx" width),
                                         Attr (unqual "height") (printf "%.0fpx" height),
                                         Attr (unqual "viewBox") (printf "0 0 %f %f" l l)],
                                        [svgRectangle color rect |
                                         (color, rect) <- zip (cycle colors) $ makeTreeMap' rect tree])
    where rect = Rectangle 0 0 l l
          l = sqrt $ treeSize tree
