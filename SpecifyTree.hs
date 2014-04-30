module SpecifyTree where

import Text.JSON

data SpecifyTreeNode = SpecifyTreeNode {
  nodeId :: Int,
  rankId :: Int,
  parentId :: Maybe Int,
  name :: String,
  count :: Int
  } deriving Show

data SpecifyTree = SpecifyTree [SpecifyTreeNode] deriving Show

testJSON = "[[1, 0, null, \"Fish\", 0], [2, 60, 15797, \"Cephalaspidomorphi\", 0]]"

fromResult (Ok v) = v
fromResult r = error $ "not ok" ++ (show r)

getVal vals n = fromResult $ readJSON $ vals !! n

instance JSON SpecifyTreeNode where
  readJSON (JSArray vals) = Ok SpecifyTreeNode {
    nodeId = (getVal vals 0),
    rankId = (getVal vals 1),
    parentId = case (getVal vals 2) of
      JSNull -> Nothing
      v -> Just $ fromResult $ readJSON v,

    name = (getVal vals 3),
    count = (getVal vals 4)
    }

  showJSON = undefined

instance JSON SpecifyTree where
  readJSON (JSArray rows) = Ok $ SpecifyTree $ map (fromResult . readJSON) rows

  showJSON = undefined