module CTM where

import Text.Printf
import Data.List
import Text.XML.Light

colors = ["White", "Silver", "Gray", "Red", "Maroon", "Yellow", "Olive",
          "Lime", "Green", "Aqua", "Teal", "Blue", "Navy", "Fuchsia", "Purple"]

data Rectangle = Rectangle {
  rectangleX,
  rectangleY,
  rectangleW,
  rectangleH,
  rectangleB :: Double
  } deriving (Show, Eq)

data Dir = X | Y deriving (Show, Eq)

data Tree = Tree Double [Tree]
          deriving (Show)

treeSize (Tree size _) = size

partitionRect :: Dir -> Rectangle -> Double -> [Double] -> [Rectangle]
partitionRect d (Rectangle x y w h b) s0 ss = tail $ scanl (shiftRect d) r0 ws
  where r0 = if d == X then Rectangle 0 y x h b else Rectangle x 0 w y b
        w0  = if d == X then w else h
        ws = map (\s -> w0*s/s0) ss
        shiftRect X (Rectangle x y w h b) w' = Rectangle (x + w) y       w' h   (0.5*b)
        shiftRect Y (Rectangle x y w h b) h' = Rectangle x       (y + h) w  h'  (0.5*b)

ctm :: Dir -> Rectangle -> Tree -> [Rectangle]
ctm _ _ (Tree 0  _) = []
ctm d r (Tree _ []) = [r]
ctm d r (Tree size children) = r : (concat $ map (\(r, c) -> ctm d' r c) (zip rs children))
  where ss = map treeSize children
        rs = partitionRect d r size ss
        d' = if d == X then Y else X


svgRectangle :: String -> Rectangle -> Element
svgRectangle color (Rectangle x y w h b) =
  unode "rect" [Attr (unqual "x") (printf "%.2f" x),
                Attr (unqual "y") (printf "%.2f" y),
                Attr (unqual "width") (printf "%.2f" w),
                Attr (unqual "height") (printf "%.2f" h),
                -- Attr (unqual "stroke-width") "1",
                -- Attr (unqual "stroke") "black",
                Attr (unqual "fill") color
                -- Attr (unqual "fill-opacity") "0.2"
               ]

svgCtm :: Double -> Double -> Tree -> Element
svgCtm width height tree = unode "svg" ([Attr (unqual "xmlns") "http://www.w3.org/2000/svg",
                                         Attr (unqual "width") (printf "%.0fpx" width),
                                         Attr (unqual "height") (printf "%.0fpx" height)],
                                        (map (\(color, rect) -> svgRectangle color rect) $
                                         zip (cycle colors) $
                                         sortBy borderSort $
                                         filter (\(Rectangle _ _ w h _) -> w >= 4 && h >= 4) $
                                         ctm X (Rectangle 0 0 width height 1) tree))
  where borderSort r0 r1 = compare (rectangleB r1) (rectangleB r0)
