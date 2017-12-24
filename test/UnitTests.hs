import           Data.EulerTourTree

import           Data.Tree
import           Test.Tasty
import           Test.Tasty.HUnit


-- Unsafe version of fromTree
fromTree' :: Ord node => Tree node -> EulerTourTree node
fromTree' tree = let Just a = fromTree tree in a

main :: IO ()
main = defaultMain $ testGroup "Unit tests"
  [ cutEdgeTest
  , memberTest
  , rerootTest
  , rootTest
  , sizeTest
  , spliceTest
  ]

testTree1 :: EulerTourTree Int
testTree1 = fromTree' $ Node 0
  [ Node 10
    [ Node 11 []
    , Node 12 [] ]
  , Node 20
    [ Node 21 []
    , Node 22 [] ] ]

testTree2 :: EulerTourTree Int
testTree2 = fromTree' $ Node 100
  [ Node 110
    [ Node 111 []
    , Node 112 []
    ]
  , Node 120
    [ Node 121 []
    , Node 122 []
    ]
  ]


cutEdgeTest :: TestTree
cutEdgeTest = testCase "cutEdge" $ do
  cutEdge empty (0, 1) @?= Nothing
  cutEdge testTree1 (10, 11) @?= Just (fromTree' $ Node 11 [], fromTree' $ Node 0
   [ Node 10
     [ Node 12 [] ]
   , Node 20
     [ Node 21 []
     , Node 22 [] ] ])

memberTest :: TestTree
memberTest = testCase "member" $ do
  member 0 testTree1  @?= True
  member 1 testTree1  @?= False
  member 12 testTree1 @?= True
  member 21 testTree1 @?= True
  member 23 testTree1 @?= False

rerootTest :: TestTree
rerootTest = testCase "reroot" $ do
  reroot 0 empty @?= Nothing
  reroot 12 testTree1 @?= Just (fromTree' $ Node 12
    [ Node 10
      [ Node 0
        [ Node 20
          [ Node 21 []
          , Node 22 [] ] ]
        , Node 11 [] ] ] )

rootTest :: TestTree
rootTest = testCase "root" $ do
  root (empty :: EulerTourTree Int) @?= Nothing
  root (singleton 1) @?= Just 1
  root testTree1 @?= Just 0

sizeTest :: TestTree
sizeTest = testCase "size" $ do
  size (empty :: EulerTourTree Int) @?= 0
  size (singleton 1) @?= 1
  size testTree1 @?= 7
  size testTree2 @?= 7

spliceTest :: TestTree
spliceTest = testCase "splice" $ do
  splice testTree2 12 empty @?= Nothing
  splice testTree2 12 testTree1 @?= Just (fromTree' $ Node 0
    [ Node 10
      [ Node 11 []
      , Node 12
          [ Node 100
            [ Node 110
              [ Node 111 []
              , Node 112 [] ]
            , Node 120
              [ Node 121 []
              , Node 122 [] ] ] ] ]
    , Node 20
      [ Node 21 []
      , Node 22 [] ] ] )
