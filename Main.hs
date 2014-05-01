module Main where

import Text.XML.Light
import SpecifyTree
import CTM

main = do
  treeData <- readFile "taxon_data.json"
  let tree = treeFromJson treeData
  let svg = svgCtm 600 600 tree
  putStrLn $ ppElement svg
