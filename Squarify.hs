module Squarify where

import Data.List
import Data.Tree
import Text.Printf
import Text.XML.Light
import GHC.Exts (sortWith)

type Length = Double
type Area = Double
type RectId = String
type Color = String
type ElName = String

data Rectangle = Rectangle { x, y, w, h :: Length } deriving (Show, Eq)

colors = ["Silver", "Gray", "Red", "Maroon", "Yellow", "Olive",
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

treeSize :: Tree Int -> Double
treeSize (Node s _) = fromIntegral s

children :: Tree a -> [Tree a]
children (Node _ c) = c

flattenTree :: Tree a -> [a]
flattenTree (Node v children) = concatMap flattenTree children ++ [v]

makeTreeMap :: Rectangle -> Tree Int -> Tree Rectangle
makeTreeMap rect tree = Node rect [makeTreeMap r t | (r, t) <- zip rs subtrees]
  where rs = squarify rect areas []
        areas = map treeSize subtrees
        subtrees = sortWith (\c -> -1 * treeSize c) $
                   filter (\c -> 0 < treeSize c) $
                   children tree

rectsToEls :: Tree Rectangle -> String -> [String] -> Tree Element
rectsToEls (Node rect children) id colors = Node element els
  where element = svgRectangle id (head colors) rect
        -- color = case children of
        --   [] -> head colors
        --   _  -> "none"
        els = [rectsToEls tree (id ++ "-" ++ show i) colors' |
               (tree, i, colors') <- zip3 children [1..] (tail $ tails colors)]

svgRectangle :: String -> Color -> Rectangle -> Element
svgRectangle id color (Rectangle x y w h) =
  unode "rect" [Attr (unqual "id") id,
                 Attr (unqual "x") (printf "%.2f" x),
                 Attr (unqual "y") (printf "%.2f" y),
                 Attr (unqual "rx") (printf "%.2f" $ 0.01 * w),
                 Attr (unqual "ry") (printf "%.2f" $ 0.01 * h),
                 Attr (unqual "width") (printf "%.2f" w),
                 Attr (unqual "height") (printf "%.2f" h),
                 -- Attr (unqual "stroke-width") (printf "%.2f" $ (*) 0.03 $ min w h),
                 -- Attr (unqual "stroke") "black",
                 Attr (unqual "fill") color
                 -- Attr (unqual "fill-opacity") "0.2"
               ]

svgStm :: Double -> Double -> Tree Int -> Element
svgStm width height tree = unode "svg" ([Attr (unqual "xmlns") "http://www.w3.org/2000/svg",
                                         Attr (unqual "width") (printf "%.0fpx" width),
                                         Attr (unqual "height") (printf "%.0fpx" height),
                                         Attr (unqual "viewBox") (printf "0 0 %f %f" l l)],
                                        concat $ levels elements)
    where
      elements = rectsToEls (makeTreeMap rect tree) "r" (cycle colors)
      rect = Rectangle 0 0 l l
      l = sqrt $ treeSize tree
