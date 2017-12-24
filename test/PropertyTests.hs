import           Data.EulerTourTree

import           Data.Foldable
import           Data.Key
import           Data.Monoid
import           Data.Sequence         hiding (empty)
import           Data.Tree
import           Test.QuickCheck.Gen
import           Test.Tasty
import           Test.Tasty.QuickCheck


-- Unsafe version of fromTree
fromTree' :: Ord node => Tree node -> EulerTourTree node
fromTree' tree = let Just a = fromTree tree in a


main :: IO ()
main = defaultMain $ testGroup "Property tests"
  [ fromToTree
  , spliceEmpty
  ]

fromToTree :: TestTree
fromToTree = testProperty "toTree . fromTree == id" $ do
  tree <- genTree
  let intermediate = fromTree tree
      result = toTree =<< intermediate
  return $ counterexample (show tree <> "\n" <> show intermediate <> "\n" <> show result) $
    Just tree == result

spliceEmpty :: TestTree
spliceEmpty = testProperty "splice empty == id" $ do
  tree <- fromTree' <$> genTree
  node <- genElement tree
  let result = splice empty node tree
  return $ counterexample (show tree <> "\n" <> show node <> "\n" <> show result) $ Just tree == result


genElement :: Foldable f => f a -> Gen a
genElement = elements . toList

genTree :: Gen (Tree (Seq Int))
genTree = genTreeDepth =<< elements [1..1]

genTreeDepth :: Int -> Gen (Tree (Seq Int))
genTreeDepth n = mapWithKey (\key _ -> key) <$> genTreeDepth' n

genTreeDepth' :: Int -> Gen (Tree ())
genTreeDepth' 0 = return $ Node () []
genTreeDepth' n = Node () <$> vectorOf 5 (genTreeDepth' $ n-1)
