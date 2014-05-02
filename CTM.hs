module CTM where

import Text.Printf
import GHC.Float
import Data.List
import Data.Map (fromList, findWithDefault)
import Text.XML.Light
import Codec.Picture

colors = ["White", "Silver", "Gray", "Red", "Maroon", "Yellow", "Olive",
          "Lime", "Green", "Aqua", "Teal", "Blue", "Navy", "Fuchsia", "Purple"]

data Surface = Surface Double Double Double Double

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

f = 0.75

ctm :: Rectangle -> Tree -> [(Rectangle, Surface)]
ctm r t = ctmSub True 0.5 X r (Surface 0 0 0 0) t

ctmSub :: Bool -> Double -> Dir -> Rectangle -> Surface -> Tree -> [(Rectangle, Surface)]
ctmSub _ _ _ _ _ (Tree 0  _) = []
ctmSub root h d r s (Tree _ []) = [(r, if root then s else addRidge h d r s)]
ctmSub root h d r s (Tree size children) = concat $ map (\(r, c) -> ctmSub False (f*h) d' r s' c) $ zip rs children
  where ss = map treeSize children
        rs = partitionRect d r size ss
        d' = if d == X then Y else X
        s' = if root then s else addRidge h d r s

addRidge :: Double -> Dir -> Rectangle -> Surface -> Surface
addRidge h X r s = Surface s1' s2' sy1 sy2
  where Rectangle x _ w _ _ = r
        Surface s1 s2 sy1 sy2 = s
        (s1', s2' ) = addRidgeSub h x w s1 s2

addRidge h Y r s = Surface sx1 sx2 s1' s2'
  where Rectangle _ x _ w _ = r
        Surface sx1 sx2 s1 s2 = s
        (s1', s2') = addRidgeSub h x w s1 s2

addRidgeSub h x w s1 s2 = ((s1 + 4*h*(x + x + w)/w), (s2 - 4*h/w))

renderCushion :: (Rectangle, Surface) -> [(Int, Int, Double)]
renderCushion (Rectangle x y w h _, Surface x1 x2 y1 y2) =
  [(ix, iy, p (fromIntegral ix) (fromIntegral iy)) |
   ix <- [(truncate (x + 0.5)) .. (truncate (x + w - 0.5))],
   iy <- [(truncate (y + 0.5)) .. (truncate (y + h - 0.5))]]
  where p ix iy = ia + max 0 (is * cosa)
          where cosa = (nx*lx + ny*ly + lz) / sqrt (nx*nx + ny*ny + 1.0)
                nx = - (2*x2*(ix + 0.5) + x1)
                ny = - (2*y2*(iy + 0.5) + y1)
        ia = 40
        is = 215
        lx = 0.09759
        ly = 0.19518
        lz = 0.9759

imageCtm :: Int -> Int -> Tree -> Image Pixel8
imageCtm w h tree = generateImage fromPixels w h
  where pixels = fromList [((x, y), (round p)) | (x,y,p) <- ps]
        fromPixels x y = findWithDefault 0 (x, y) pixels
        ps = concat $ map renderCushion $
             ctm (Rectangle 0 0 (fromIntegral w) (fromIntegral h) 1) tree

writePngCtm :: Int -> Int -> Tree -> FilePath -> IO ()
writePngCtm w h tree path = writePng path $ imageCtm w h tree

-- svgRectangle :: String -> Rectangle -> Element
-- svgRectangle color (Rectangle x y w h b) =
--   unode "rect" [Attr (unqual "x") (printf "%.2f" x),
--                 Attr (unqual "y") (printf "%.2f" y),
--                 Attr (unqual "width") (printf "%.2f" w),
--                 Attr (unqual "height") (printf "%.2f" h),
--                 -- Attr (unqual "stroke-width") "1",
--                 -- Attr (unqual "stroke") "black",
--                 Attr (unqual "fill") color
--                 -- Attr (unqual "fill-opacity") "0.2"
--                ]

-- svgCtm :: Double -> Double -> Tree -> Element
-- svgCtm width height tree = unode "svg" ([Attr (unqual "xmlns") "http://www.w3.org/2000/svg",
--                                          Attr (unqual "width") (printf "%.0fpx" width),
--                                          Attr (unqual "height") (printf "%.0fpx" height)],
--                                         (map (\(color, rect) -> svgRectangle color rect) $
--                                          zip (cycle colors) $
--                                          sortBy borderSort $
--                                          filter (\(Rectangle _ _ w h _) -> w >= 4 && h >= 4) $
--                                          ctm X (Rectangle 0 0 width height 1) tree))
--   where borderSort r0 r1 = compare (rectangleB r1) (rectangleB r0)
