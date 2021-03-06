module Main where

import Text.XML.Light
import SpecifyTree
import Squarify

main = do
  treeData <- readFile "taxon_data.json"
  let tree = treeFromJson treeData
  let svg = svgStm 600 600 tree
  putStrLn $ ppElement svg
  -- writePngCtm 1000 1000 tree "test.png"