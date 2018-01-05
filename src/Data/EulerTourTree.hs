{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
-- | Implementation of Euler tour trees.
--
-- > import qualified Data.EulerTourTree as ETTree
-- > import Data.EulerTourTree(EulerTourTree)
--
-- When specified, time complexity refers to the number /n/ of nodes in the input tree.
module Data.EulerTourTree
  ( EulerTourTree
    -- * Construction
  , empty
  , singleton
  , fromTree
    -- * Deconstruction
  , toTree
    -- * Query
  , root
  , member
  , size
    -- * Transformation
  , cutEdge
  , splice
  , reroot
  ) where

-- {{{ Imports
import           Control.Applicative             ((<|>))
import           Control.Applicative.Combinators (endBy)
import           Control.Monad                   (MonadPlus (..), guard)
import           Control.Monad.State.Lazy        (MonadState (..), evalStateT)
import           Data.FingerTree                 hiding (empty, null, singleton)
import qualified Data.FingerTree                 as FingerTree
import           Data.Foldable                   (Foldable (..))
import           Data.List.Unique                (allUnique)
import           Data.Maybe                      (fromMaybe)
import           Data.Monoid
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
import           Data.Tree                       (Tree (..))
-- }}}

searchM :: MonadPlus m => Measured v a => (v -> v -> Bool) -> FingerTree v a -> m (FingerTree v a, a, FingerTree v a)
searchM f tree = case FingerTree.search f tree of
  Position before element after -> return (before, element, after)
  _                             -> mzero

initSafe :: Measured v a => FingerTree v a -> FingerTree v a
initSafe tree = case viewr tree of
  result :> _ -> result
  _           -> tree

tailSafe :: Measured v a => FingerTree v a -> FingerTree v a
tailSafe tree = case viewl tree of
  _ :< result -> result
  _           -> tree

newtype EulerTourNode node = EulerTourNode node

deriving instance (Eq node) => Eq (EulerTourNode node)
deriving instance (Ord node) => Ord (EulerTourNode node)
deriving instance (Show node) => Show (EulerTourNode node)

data EulerTourMonoid node = EulerTourMonoid
  (First node)
  (Set (node, node))
  (Last node)
  (Set node)
  (Sum Int)

deriving instance Show node => Show (EulerTourMonoid node)

instance Ord node => Monoid (EulerTourMonoid node) where
  mempty = EulerTourMonoid mempty mempty mempty mempty mempty
  EulerTourMonoid a b c d e `mappend` EulerTourMonoid a' b' c' d' e' = result where
    result = EulerTourMonoid (a <> a') (b <> bMiddle <> b') (c <> c') (d <> d') (e <> e')
    bMiddle = fromMaybe mempty $ do
      l <- getLast c
      f <- getFirst a'
      return $ Set.singleton (min l f, max l f)

instance Ord node => Measured (EulerTourMonoid node) (EulerTourNode node) where
  measure (EulerTourNode node) = EulerTourMonoid (pure node) mempty (pure node) (Set.singleton node) (pure 1)

firstVertex :: MonadPlus m => Ord node => EulerTourMonoid node -> m node
firstVertex (EulerTourMonoid first _ _ _ _) = maybe mzero return $ getFirst first

allNodes :: Ord node => EulerTourMonoid node -> Set node
allNodes (EulerTourMonoid _ _ _ nodes _) = nodes

vertexMember :: Ord node => node -> EulerTourMonoid node -> Bool
vertexMember node (EulerTourMonoid _ _ _ nodes _) = Set.member node nodes

edgeMember :: Ord node => (node, node) -> EulerTourMonoid node -> Bool
edgeMember (u, v) (EulerTourMonoid _ edges _ _ _) = Set.member (min u v, max u v) edges

tourSize :: EulerTourMonoid node -> Int
tourSize (EulerTourMonoid _ _ _ nodes _) = Set.size nodes


-- | Euler-tour implementation of a tree structure. It is parameterized by a node type @node@.
--
-- Requirements:
--
-- - @node@ is ordered
-- - node values are unique
data EulerTourTree node where
  EulerTourTree :: Ord node => FingerTree (EulerTourMonoid node) (EulerTourNode node) -> EulerTourTree node

instance Ord node => Measured (Set (node, node), Set node, Sum Int) (EulerTourTree node) where
  measure (EulerTourTree tree) = (edges, nodes, size) where
    EulerTourMonoid _ edges _ nodes size = measure tree

-- | 'member' and 'size' are respectively faster than 'elem' and 'length' (which run in /O(n)/).
instance Foldable EulerTourTree where
  foldMap f etTree@(EulerTourTree _) = maybe mempty (foldMap f) $ toTree etTree

deriving instance Eq node => Eq (EulerTourTree node)
deriving instance Ord node => Ord (EulerTourTree node)
deriving instance Show node => Show (EulerTourTree node)


-- | /O(1)/ Construct an empty Euler tour tree.
empty :: Ord node => EulerTourTree node
empty = EulerTourTree FingerTree.empty

-- | /O(1)/ Construct an Euler tour tree with a single element.
singleton :: Ord node => node -> EulerTourTree node
singleton node = EulerTourTree $ FingerTree.singleton $ EulerTourNode node

-- | /O(n)/ Construct an Euler tour tree from a 'Data.Tree'.
--
-- Fail if, and only if, node values are not unique.
fromTree :: MonadPlus m => Ord node => Tree node -> m (EulerTourTree node)
fromTree tree = do
  guard $ allUnique $ toList tree
  return $ EulerTourTree $ fromTree' tree
  where fromTree' (Node node forest) = EulerTourNode node <| mconcat (map ((\x -> x |> EulerTourNode node) . fromTree') forest)

type Parser node m = (MonadState (FingerTree (EulerTourMonoid node) (EulerTourNode node)) m, MonadPlus m)

-- | /O(n)/ Deconstruct an Euler tour tree into a 'Data.Tree'.
toTree :: MonadPlus m => Ord node => EulerTourTree node -> m (Tree node)
toTree (EulerTourTree fingerTree) = evalStateT parser fingerTree where
  parser = do
    EulerTourNode node <- anyToken
    forest <- parser `endBy` try (token $ EulerTourNode node)
    return $ Node node forest
  anyToken :: Ord node => Parser node m => m (EulerTourNode node)
  anyToken = do
    tree <- get
    case viewl tree of
      node :< tree' -> put tree' >> return node
      _             -> mzero
  token x = do
    t <- anyToken
    guard (t == x)
    return t
  try f = do
    a <- get
    f <|> (put a >> mzero)

-- | /O(1)/ Return the root of the tree. Fail if it is empty.
root :: MonadPlus m => Ord node => EulerTourTree node -> m node
root (EulerTourTree tree) = firstVertex $ measure tree

-- | /O(log n)/ Return 'True' if node is an element of the tree.
member :: Ord node => node -> EulerTourTree node -> Bool
member node (EulerTourTree tree) = vertexMember node $ measure tree

-- | /O(1)/ Return the number of elements in the tree.
size :: Ord node => EulerTourTree node -> Int
size (EulerTourTree fingerTree) = tourSize $ measure fingerTree

-- | /O(log n)/ Return 2 subtrees of @tree@ where @a@ is the subtree of nodes __a__bove @edge@, and @b@ is the subtree of nodes __b__elow @edge@.
--
-- Fail if @edge@ isn't found in @tree@
cutEdge :: MonadPlus m
        => Ord node
        => EulerTourTree node  -- ^ Denoted by @tree@
        -> (node, node)        -- ^ Denoted by @edge@
        -> m (EulerTourTree node, EulerTourTree node)  -- ^ Denoted by @(a, b)@
cutEdge (EulerTourTree tree) e@(a, b) = do
  (left, node, tree') <- searchM p1 tree
  (middle, node', right) <- searchM p2 $ node <| tree'
  let inside = node <| middle
      outside = left >< tailSafe right
  return (EulerTourTree inside, EulerTourTree outside)
  where p1 before after = edgeMember e before
        p2 before after = not (edgeMember e after)

-- | /O(log n)/ Attach @tree1@ as a child of @node@ in @tree2@.
--
-- Fail if @node@ isn't found in @tree2@, or if @tree1@ and @tree2@ have nodes in common.
splice :: MonadPlus m
       => Ord node
       => EulerTourTree node           -- ^ Denoted by @tree1@
       -> node                         -- ^ Denoted by @node@
       -> EulerTourTree node           -- ^ Denoted by @tree2@
       -> m (EulerTourTree node)
splice (EulerTourTree inviteeTree) node (EulerTourTree hostTree) = do
  guard $ null $ Set.intersection (allNodes $ measure inviteeTree) (allNodes $ measure hostTree)
  (left, _, right) <- searchM p hostTree
  let inviteeTree' = if FingerTree.null inviteeTree then inviteeTree else EulerTourNode node <| inviteeTree
  return $ EulerTourTree $ left >< inviteeTree' >< (EulerTourNode node <| right)
  where p before after = vertexMember node before && not (vertexMember node after)

-- | /O(log n)/ Rotate @tree@ such that @node@ is the new root.
--
-- Fail if @node@ isn't found in @tree@.
reroot :: MonadPlus m
       => Ord node
       => node                 -- ^ Denoted by @node@
       -> EulerTourTree node   -- ^ Denoted by @tree@
       -> m (EulerTourTree node)
reroot node (EulerTourTree tree) = do
  (left, _, tree') <- searchM p1 tree
  (middle, _, right) <- searchM p2 (etNode <| tree')
  return $ EulerTourTree $ middle >< (etNode <| initSafe right) >< (left |> etNode)
  where p1 before after = vertexMember node before
        p2 before after = not (vertexMember node after)
        etNode = EulerTourNode node
