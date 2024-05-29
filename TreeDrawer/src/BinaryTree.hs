{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}
module BinaryTree
  ( TreeNode (..),
    add,
  )
where

type TreeNodeValue = Int

type TreeLevel = Int

type TreeRoot = TreeNode

data TreeNode = TreeNode
  { value :: TreeNodeValue,
    left :: Maybe TreeNode,
    right :: Maybe TreeNode,
    level :: TreeLevel
  }

createEmptyNode :: TreeLevel -> TreeNodeValue -> TreeNode
createEmptyNode level value =
  TreeNode
    { value = value,
      left = Nothing,
      right = Nothing,
      level = level
    }

addWithLevel :: TreeLevel -> TreeNodeValue -> Maybe TreeRoot -> TreeNode
addWithLevel treeLevel newValue maybeRoot = do
  case maybeRoot of
    Nothing ->
      createEmptyNode
        treeLevel
        newValue        
    Just root ->
      case newValue < value root of
        True -> root {left = Just $ addWithLevel (level root + 1) newValue (left root)}
        False -> root {right = Just $ addWithLevel (level root + 1) newValue (right root)}

add :: TreeNodeValue -> Maybe TreeRoot -> TreeNode
add = addWithLevel 0