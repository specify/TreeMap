module SpecifyTree where

import Control.Applicative
import Data.Map (Map, fromList, fromListWith, findWithDefault, (!))
import Text.JSON
import CTM

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

makeTree :: NodesById -> NodesByParent -> Maybe Family -> Maybe NodeID -> Tree
makeTree byId byParent family nId = Tree family' size children
  where size = sum [size | (Tree _ size _) <- children]
        children = if thisSize > 0
                   then childForThis : actualChildren
                   else actualChildren
        thisSize = case nId of
          Nothing  -> 0
          Just nId -> count $ byId ! nId
        childForThis = Tree family' (fromIntegral thisSize) []
        actualChildren = map (treeFromNodeId . Just . nodeId) childNodes
        treeFromNodeId nId = makeTree byId byParent family' nId
        childNodes = findWithDefault [] nId byParent
        family' = case nId of
          Nothing  -> Nothing
          Just nId -> if 100 == (rankId $ byId ! nId)
                      then Just $ name $ byId ! nId
                      else family

specifyToTree :: SpecifyTree -> Tree
specifyToTree specifyTree = makeTree byId byParent Nothing Nothing
  where byId = nodesById specifyTree
        byParent = groupByParent specifyTree

treeFromJson :: String -> Tree
treeFromJson s = case (decode s) of
  Ok st -> specifyToTree st
  _     -> error "couldn't parse json"
