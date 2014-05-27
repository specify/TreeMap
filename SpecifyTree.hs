module SpecifyTree where

import Control.Applicative
import Data.Map (Map, fromList, fromListWith, findWithDefault, (!))
import Data.Tree
import Text.JSON

type NodeID = Int
type RankID = Int
type ParentID = Maybe NodeID

data SpecifyTreeNode = SpecifyTreeNode {
  nodeId :: NodeID,
  rankId :: RankID,
  parentId :: ParentID,
  name :: String,
  count :: Int
  } deriving Show

data SpecifyTree = SpecifyTree [SpecifyTreeNode] deriving Show

instance JSON SpecifyTreeNode where
  readJSON (JSArray row) = do
    nodeId   <- getVal row 0
    rankId   <- getVal row 1

    parentId <- case row !! 2 of
      JSNull -> Ok Nothing
      v      -> Just <$> readJSON v

    name     <- getVal row 3
    count    <- getVal row 4
    return $ SpecifyTreeNode nodeId rankId parentId name count

    where getVal vals n = readJSON $ vals !! n

  showJSON = undefined

instance JSON SpecifyTree where
  readJSON (JSArray rows) = SpecifyTree <$> mapM readJSON rows

  showJSON = undefined

type NodesByParent = Map ParentID [SpecifyTreeNode]

groupByParent :: SpecifyTree -> NodesByParent
groupByParent (SpecifyTree nodes) =
  fromListWith (++) [(parentId n, [n]) | n <- nodes]

type NodesById = Map NodeID SpecifyTreeNode

nodesById :: SpecifyTree -> NodesById
nodesById (SpecifyTree nodes) = fromList [(nodeId n, n) | n <- nodes]

makeTree :: NodesById -> NodesByParent  -> Maybe NodeID -> Tree Int
makeTree byId byParent nId = case children of
  [child]  | thisSize == 0 -> child
  children | thisSize  > 0 -> Node size $ (Node thisSize []):children
  children                 -> Node size children
  where size = thisSize + sum [size | (Node size _) <- children]
        thisSize = case nId of
          Nothing  -> 0
          Just nId -> count $ byId ! nId
        children = map (treeFromNodeId . Just . nodeId) childNodes
        treeFromNodeId nId = makeTree byId byParent nId
        childNodes = findWithDefault [] nId byParent

specifyToTree :: SpecifyTree -> Tree Int
specifyToTree specifyTree = makeTree byId byParent Nothing
  where byId = nodesById specifyTree
        byParent = groupByParent specifyTree

treeFromJson :: String -> Tree Int
treeFromJson s = case (decode s) of
  Ok st -> specifyToTree st
  _     -> error "couldn't parse json"
