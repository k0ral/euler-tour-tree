{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}
-- | EXPERIMENTAL
module Data.EulerTourTree.Sized where

import qualified Data.EulerTourTree as Unsized

import           Control.Monad
import           Data.FingerTree    as FingerTree
import           Data.Monoid
import           Data.Set           as Set
import           GHC.TypeLits

-- | Statically sized version of 'Unsized.EulerTourTree'
newtype EulerTourTree (n :: Nat) node = EulerTourTree (Unsized.EulerTourTree node)

deriving instance Ord node => Measured (Set (node, node), Set node, Sum Int) (EulerTourTree n node)


-- | O(1) Construct a spanning tree with a single element.
singleton :: Ord node => node -> EulerTourTree 1 node
singleton = EulerTourTree . Unsized.singleton

cutEdge :: MonadPlus m => Ord node => ((p + q) ~ n)
        => EulerTourTree n node
        -> (node, node)
        -> m (EulerTourTree p node, EulerTourTree q node)
cutEdge (EulerTourTree tree) edge = do
  (inside, outside) <- Unsized.cutEdge tree edge
  return (EulerTourTree inside, EulerTourTree outside)

splice :: MonadPlus m => Ord node => EulerTourTree p node -> node -> EulerTourTree q node -> m (EulerTourTree (p+q) node)
splice (EulerTourTree invitee) node (EulerTourTree host) = EulerTourTree <$> Unsized.splice invitee node host

reroot :: MonadPlus m => Ord node => node -> EulerTourTree n node -> m (EulerTourTree n node)
reroot node (EulerTourTree tree) = EulerTourTree <$> Unsized.reroot node tree

-- | O(1) Return root of the tree.
root :: MonadPlus m => Ord node => EulerTourTree n node -> m node
root (EulerTourTree tree) = Unsized.root tree
