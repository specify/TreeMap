module SpecifyTree where

import Data.Map (Map, fromList, fromListWith, findWithDefault, (!))
import Text.JSON
import CTM

data SpecifyTreeNode = SpecifyTreeNode {
  nodeId :: Int,
  rankId :: Int,
  parentId :: Int,
  name :: String,
  count :: Int
  } deriving Show

data SpecifyTree = SpecifyTree [SpecifyTreeNode] deriving Show

fromResult (Ok v) = v
fromResult r = error $ "not ok" ++ (show r)

getVal vals n = fromResult $ readJSON $ vals !! n

instance JSON SpecifyTreeNode where
  readJSON (JSArray vals) = Ok SpecifyTreeNode {
    nodeId = (getVal vals 0),
    rankId = (getVal vals 1),
    parentId = case (getVal vals 2) of
      JSNull -> (-1)
      v -> fromResult $ readJSON v,

    name = (getVal vals 3),
    count = (getVal vals 4)
    }

  showJSON = undefined

instance JSON SpecifyTree where
  readJSON (JSArray rows) = Ok $ SpecifyTree $ map (fromResult . readJSON) rows

  showJSON = undefined

groupByParent :: SpecifyTree -> Map Int [SpecifyTreeNode]
groupByParent (SpecifyTree nodes) = fromListWith (++) [(parentId n, [n]) | n <- nodes]

nodesById :: SpecifyTree -> Map Int SpecifyTreeNode
nodesById (SpecifyTree nodes) = fromList [(nodeId n, n) | n <- nodes]

specifyToTree :: SpecifyTree -> Tree
specifyToTree specifyTree = treeFromNodeId byId byParent (-1)
  where byId = nodesById specifyTree
        byParent = groupByParent specifyTree

treeFromNodeId :: Map Int SpecifyTreeNode -> Map Int [SpecifyTreeNode] -> Int -> Tree
treeFromNodeId byId byParent nId = Tree size childs
  where thisSize = if nId == (-1) then 0 else (count $ byId ! nId)
        childNodes = findWithDefault [] nId byParent
        childs' = map (treeFromNodeId byId byParent) $ map nodeId childNodes
        childs = if thisSize > 0 then (Tree (fromIntegral thisSize) []) : childs' else childs'
        size = sum $ map (\(Tree size _) -> size) childs

treeFromJson :: String -> Tree
treeFromJson s = case (decode s) of
  (Ok st) -> specifyToTree st
  _ -> error "couldn't parse json"
