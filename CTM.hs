import Text.XML.Light

data Rectangle = Rectangle Double Double Double Double
               deriving (Show, Eq)

data Surface = Surface Double Double Double Double
             deriving (Show, Eq)

data Dir = X | Y deriving (Show, Eq)

data Cushion = Cushion Rectangle Surface
             deriving (Show, Eq)

data Tree = Tree Double [Tree]
          deriving (Show)

treeSize (Tree size _) = size

partitionRect :: Dir -> Rectangle -> Double -> [Double] -> [Rectangle]
partitionRect d (Rectangle x0 y0 x1 y1) s0 ss = tail $ scanl (shiftRect d) r0 ws
  where r0 = if d == X then Rectangle 0 y0 x0 y1 else Rectangle x0 0 x1 y0
        w  = if d == X then x1 - x0 else y1 - y0
        ws = map (\s -> w*s/s0) ss
        shiftRect X (Rectangle _ _ x _) w = Rectangle x  y0 (x + w) y1
        shiftRect Y (Rectangle _ _ _ y) w = Rectangle x0 y  x1      (y + w)

ctm :: Dir -> Rectangle -> Tree -> [Rectangle]
ctm d r (Tree _ []) = [r]
ctm d r (Tree size children) = r : (concat $ map (\(r, c) -> ctm d' r c) (zip rs children))
  where ss = map treeSize children
        rs = partitionRect d r size ss
        d' = if d == X then Y else X


svgRectangle :: Rectangle -> Element
svgRectangle (Rectangle x0 y0 x1 y1) = unode "rect" [Attr (unqual "x") (show x0),
                                                     Attr (unqual "y") (show y0),
                                                     Attr (unqual "width") (show (x1 - x0)),
                                                     Attr (unqual "height") (show (y1 - y0)),
                                                     Attr (unqual "stroke") "black",
                                                     Attr (unqual "fill") "white"]
svgCtm :: Double -> Double -> Tree -> Element
svgCtm width height tree = unode "svg" ([Attr (unqual "width") (show width),
                                        Attr (unqual "height") (show height)],
                                        (map svgRectangle $ ctm X (Rectangle 0 0 width height) tree))
