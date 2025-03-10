{- DO NOT CHANGE MODULE NAME, if you do, the file will not load properly -}
module Coursework where

import Data.List
import qualified Data.Set as HS (fromList, toList)
import Test.QuickCheck

{-
  Your task is to design a datatype that represents the mathematical concept of
  a (finite) set of elements (of the same type). We have provided you with an
  interface (do not change this!) but you will need to design the datatype and
  also support the required functions over sets. Any functions you write should
  maintain the following invariant: no duplication of set elements.

  There are lots of different ways to implement a set. The easiest is to use a
  list. Alternatively, one could use an algebraic data type, wrap a binary
  search tree, or even use a self-balancing binary search tree. Extra marks will
  be awarded for efficient implementations (a self-balancing tree will be more
  efficient than a linked list for example).

  You are **NOT** allowed to import anything from the standard library or other
  libraries. Your edit of this file should be completely self-contained.

  **DO NOT** change the type signatures of the functions below: if you do, we
  will not be able to test them and you will get 0% for that part. While sets
  are unordered collections, we have included the Ord constraint on some
  signatures: this is to make testing easier.

  You may write as many auxiliary functions as you need. Everything must be in
  this file.

  See the note **ON MARKING** at the end of the file.
-}

{-
   PART 1.
   You need to define a Set datatype.
-}

-- AVL tree - self-balancing binary search tree
data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a) Int

-- you **MUST** change this to your own data type. The declaration of Set a =
-- Int is just to allow you to load the file into ghci without an error, it
-- cannot be used to represent a set.
newtype Set a = Set { unSet :: BinaryTree a }

-- prints the the self-balancing binary search tree sideways, also showing the height and balance factor of each node
-- example usage: putStr $ printSet $ fromList [1,4,2,3,9,8,6,7,5]
-- doesn't currently work with printing out powersets
-- powersets can be shown as a list using: map toList $ toList $ powerSet $ fromList [1..4]
printSet :: Show a => Set a -> String
printSet s = printTree (unSet s) 0
    where
        printTree Empty _ = ""
        printTree (Node x left right height) indent =
            printTree right (indent + 10) ++
            replicate indent ' ' ++ show x ++ "(" ++ show height ++ "/" ++ show (treeBalanceFactor (Node x left right height)) ++ ")\n" ++
            printTree left (indent + 10)

{-
   PART 2.
   If you do nothing else, you must get the toList, fromList and equality working. If they
   do not work properly, it is impossible to test your other functions, and you
   will fail the coursework!
-}

rightRotate :: BinaryTree a -> BinaryTree a
rightRotate (Node x (Node y yLeft yRight _) xRight _) = createTreeNode y yLeft (createTreeNode x yRight xRight)

leftRotate :: BinaryTree a -> BinaryTree a
leftRotate (Node x xLeft (Node y yLeft yRight _) _) = createTreeNode y (createTreeNode x xLeft yLeft) yRight

leftRightRotate :: BinaryTree a -> BinaryTree a
leftRightRotate (Node x left right _) = rightRotate $ createTreeNode x (leftRotate left) right

rightLeftRotate :: BinaryTree a -> BinaryTree a
rightLeftRotate (Node x left right _) = leftRotate $ createTreeNode x left (rightRotate right)

leftSubtree :: BinaryTree a -> BinaryTree a
leftSubtree Empty = Empty
leftSubtree (Node _ left _ _) = left

rightSubtree :: BinaryTree a -> BinaryTree a
rightSubtree Empty = Empty
rightSubtree (Node _ _ right _) = right

avlBalance :: BinaryTree a -> BinaryTree a
avlBalance tree
  | treeBalanceFactor tree > 1 =
    if treeBalanceFactor (leftSubtree tree) >= 0 then rightRotate tree
    else leftRightRotate tree -- treeBalanceFactor (leftSubtree tree) < 0
  | treeBalanceFactor tree < -1 =
    if treeBalanceFactor (rightSubtree tree) <= 0 then leftRotate tree
    else rightLeftRotate tree -- treeBalanceFactor (rightSubtree tree) > 0
  | otherwise = tree

treeBalanceFactor :: BinaryTree a -> Int
treeBalanceFactor Empty = 0
treeBalanceFactor (Node _ left right _) = treeHeight left - treeHeight right

treeHeight :: BinaryTree a -> Int
treeHeight Empty = 0
treeHeight (Node _ _ _ height) = height

treeRemove :: Ord a => a -> BinaryTree a -> BinaryTree a
treeRemove _ Empty = Empty
treeRemove x (Node y left right _)
  | x < y = avlBalance $ createTreeNode y (treeRemove x left) right
  | x > y = avlBalance $ createTreeNode y left (treeRemove x right)
-- otherwise x == y
-- NOT NEEDED --> treeRemove _ (Node _ Empty Empty) = Empty
treeRemove _ (Node _ left Empty _) = left
treeRemove _ (Node _ Empty right _) = right
treeRemove _ (Node _ left right _) = avlBalance $ createTreeNode newKey left (treeRemove newKey right) -- newKey is the in order traversal successor
  where
    newKey = minKey right
    -- minKey function only works with non empty tree
    minKey (Node key Empty _ _) = key
    minKey (Node _ left _ _) = minKey left

instance (Ord a) => Eq (BinaryTree a) where
  t1 == t2 = inOrderTraversal t1 == inOrderTraversal t2

instance (Ord a) => Ord (BinaryTree a) where
  compare t1 t2 = compare (inOrderTraversal t1) (inOrderTraversal t2)

treeSubsequences :: Ord a => BinaryTree a -> BinaryTree (BinaryTree a)
treeSubsequences Empty = createTreeNode Empty Empty Empty
treeSubsequences (Node x left right _) =
  let withoutXLeft = treeSubsequences left
      withoutXRight = treeSubsequences right
      withoutX = treeFoldr treeMerge (treeMap (\t -> treeMap (treeMerge t) withoutXLeft) withoutXRight) Empty
      withX = treeMap (treeInsert x) withoutX
  in treeMerge withoutX withX

treeFoldr :: (a -> b -> b) -> BinaryTree a -> b -> b
treeFoldr _ Empty acc = acc
treeFoldr f (Node x left right _) acc = treeFoldr f left $ f x $ treeFoldr f right acc

treeMap :: Ord b => (a -> b) -> BinaryTree a -> BinaryTree b
treeMap f tree = treeMapMerge f tree Empty
  where
    treeMapMerge f Empty newTree = newTree
    treeMapMerge f (Node x left right _) newTree = treeMapMerge f right $ treeMapMerge f left $ treeInsert (f x) newTree

treeDifference :: Ord a => BinaryTree a -> BinaryTree a -> BinaryTree a
treeDifference tree1 Empty = tree1
treeDifference Empty _ = Empty
treeDifference tree1 tree2 = treeDifferenceNodes tree1 tree2 Empty
  where
    treeDifferenceNodes Empty _ newTree = newTree
    treeDifferenceNodes (Node x left right _) tree2 newTree = treeDifferenceNodes right tree2 $ treeDifferenceNodes left tree2 $ treeInsertDifference x newTree tree2
    treeInsertDifference x newTree tree2
      | treeSearch x tree2 = newTree
      | otherwise = treeInsert x newTree

treeCommon :: Ord a => BinaryTree a -> BinaryTree a -> BinaryTree a
treeCommon _ Empty = Empty
treeCommon Empty _ = Empty
treeCommon tree1 tree2 = treeCommonNodes tree1 tree2 Empty
  where
    treeCommonNodes Empty _ newTree = newTree
    treeCommonNodes (Node x left right _) tree2 newTree = treeCommonNodes right tree2 $ treeCommonNodes left tree2 $ treeInsertCommon x newTree tree2
    treeInsertCommon x newTree tree2
      | treeSearch x tree2 = treeInsert x newTree
      | otherwise = newTree

treeMerge :: Ord a => BinaryTree a -> BinaryTree a -> BinaryTree a
treeMerge tree1 Empty = tree1
treeMerge Empty tree2 = tree2
treeMerge (Node x left right _) tree2 = treeMerge right $ treeMerge left $ treeInsert x tree2

treeSize :: BinaryTree a -> Int
treeSize Empty = 0
treeSize (Node _ left right _) = 1 + treeSize left + treeSize right

treeSearch :: Ord a => a -> BinaryTree a -> Bool
treeSearch _ Empty = False
treeSearch x (Node y left right _)
  | x < y = treeSearch x left
  | x > y = treeSearch x right
  | otherwise = True

inOrderTraversal :: BinaryTree a -> [a]
inOrderTraversal Empty = []
inOrderTraversal (Node x left right _) = inOrderTraversal left ++ [x] ++ inOrderTraversal right

treeInsert :: Ord a => a -> BinaryTree a -> BinaryTree a
treeInsert x Empty = Node x Empty Empty 1
treeInsert x (Node y left right height)
  | x < y = avlBalance $ createTreeNode y (treeInsert x left) right
  | x > y = avlBalance $ createTreeNode y left (treeInsert x right)
  | otherwise = Node y left right height -- keep tree the same if the element is already in it

createTreeNode :: a -> BinaryTree a -> BinaryTree a -> BinaryTree a
createTreeNode x left right = Node x left right $ 1 + max (treeHeight left) (treeHeight right)

-- toList {2,1,4,3} => [1,2,3,4]
-- the output must be sorted.
toList :: Ord a => Set a -> [a]
toList = inOrderTraversal . unSet

-- fromList: do not forget to remove duplicates!
fromList :: Ord a => [a] -> Set a
fromList xs = Set { unSet = foldr treeInsert Empty $ reverse xs } -- the list is first reversed before elements are inserted, since the elements are inserted starting with the last element and ending with the first 

-- Make sure you satisfy this property. If it fails, then all of the functions
-- on Part 3 will also fail their tests
toFromListProp :: IO ()
toFromListProp =
  quickCheck
    ((\xs -> (HS.toList . HS.fromList $ xs) == (toList . fromList $ xs)) :: [Int] -> Bool)

-- test if two sets have the same elements (pointwise equivalent).
instance (Ord a) => Eq (Set a) where
  s1 == s2 = toList s1 == toList s2

-- you should be able to satisfy this property quite easily
eqProp :: IO ()
eqProp =
  quickCheck ((\xs -> (fromList . HS.toList . HS.fromList $ xs) == fromList xs) :: [Char] -> Bool)

{-
   PART 3. Your Set should contain the following functions. DO NOT CHANGE THE
   TYPE SIGNATURES.
-}

-- the empty set
empty :: Set a
empty = Set { unSet = Empty }

-- is it the empty set?
null :: Set a -> Bool
null (Set { unSet = Empty }) = True
null _ = False

-- build a one element Set
singleton :: a -> Set a
singleton x = Set { unSet = Node x Empty Empty 1 }

-- insert an element *x* of type *a* into Set *s* make sure there are no
-- duplicates!
insert :: (Ord a) => a -> Set a -> Set a
insert x s = Set { unSet = treeInsert x $ unSet s }

-- join two Sets together be careful not to introduce duplicates.
union :: (Ord a) => Set a -> Set a -> Set a
union s1 s2 = Set { unSet = treeMerge (unSet s1) (unSet s2) }

-- return, as a Set, the common elements between two Sets
intersection :: (Ord a) => Set a -> Set a -> Set a
-- intersection s1 s2 = fromList $ getCommon (toList s1) (toList s2) []
--   where
--     -- getCommon function only works with sorted lists
--     getCommon [] _ acc = acc
--     getCommon _ [] acc = acc
--     getCommon l1 l2 acc
--       | head l1 < head l2 = getCommon (tail l1) l2 acc
--       | head l1 > head l2 = getCommon l1 (tail l2) acc
--       | otherwise = getCommon (tail l1) (tail l2) (head l1 : acc) -- add to acc when head of both lists are equal
intersection s1 s2 = Set { unSet = treeCommon (unSet s1) (unSet s2) }

-- all the elements in *s1* not in *s2*
-- {1,2,3,4} `difference` {3,4} => {1,2}
-- {} `difference` {0} => {}
difference :: (Ord a) => Set a -> Set a -> Set a
-- difference s1 s2 = fromList $ getDifference (toList s1) (toList s2) []
--   where
--     -- getDifference function only works with sorted lists
--     getDifference [] _ acc = acc
--     getDifference l1 [] acc = acc ++ l1
--     getDifference l1 l2 acc
--       | head l1 < head l2 = getDifference (tail l1) l2 (head l1 : acc)
--       | head l1 > head l2 = getDifference l1 (tail l2) acc
--       | otherwise = getDifference (tail l1) (tail l2) acc -- if head of both lists are equal ignore them and go to the next element in both lists
difference s1 s2 = Set { unSet = treeDifference (unSet s1) (unSet s2) }

-- is element *x* in the Set s1?
member :: (Ord a) => a -> Set a -> Bool
member x = treeSearch x . unSet

-- how many elements are there in the Set?
cardinality :: Set a -> Int
cardinality = treeSize . unSet

-- apply a function to every element in the Set
setmap :: (Ord b) => (a -> b) -> Set a -> Set b
setmap f s = Set { unSet = treeMap f $ unSet s }

-- right fold a Set using a function *f*
-- applies foldr to sorted list version of set
setfoldr :: (a -> b -> b) -> Set a -> b -> b
-- setfoldr f s acc = treeFoldr f (unSet s) acc
setfoldr f = treeFoldr f . unSet

-- remove an element *x* from the set
-- return the set unaltered if *x* is not present
removeSet :: (Eq a, Ord a) => a -> Set a -> Set a -- CHANGED FUNCTION SIGNATURE TO ADD Ord a
removeSet x s = Set { unSet = treeRemove x $ unSet s }

instance (Ord a) => Ord (Set a) where
  compare s1 s2 = compare (toList s1) (toList s2)

-- powerset of a set
-- powerset {1,2} => { {}, {1}, {2}, {1,2} }
powerSet :: (Ord a) => Set a -> Set (Set a)  -- CHANGED FUNCTION SIGNATURE TO ADD Ord a
powerSet s = fromList $ map fromList $ allSubLists $ toList s
  where
    allSubLists xs = concatMap (subLists xs) [0..length xs]
      where
        subLists _ 0 = [[]]
        subLists [] _ = []
        subLists (x:xs) size = map (x :) (subLists xs (size - 1)) ++ subLists xs size
-- powerSet s = fromList $ map fromList $ subsequences $ toList s
-- powerSet s = Set { unSet = treeMap (\t -> Set { unSet = t }) $ treeSubsequences $ unSet s }

{-
   ON MARKING:

   Be careful! This coursework will be marked using QuickCheck, against
   Haskell's own Data.Set implementation. This testing will be conducted
   automatically via a marking script that tests for equivalence between your
   output and Data.Set's output. There is no room for discussion, a failing test
   means that your function does not work properly: you do not know better than
   QuickCheck and Data.Set! Even one failing test means 0 marks for that
   function. Changing the interface by renaming functions, deleting functions,
   or changing the type of a function will cause the script to fail to load in
   the test harness. This requires manual adjustment by a TA: each manual
   adjustment will lose 10% from your score. If you do not want to/cannot
   implement a function, leave it as it is in the file (with undefined).

   Marks will be lost for too much similarity to the Data.Set implementation.

   Pass: creating the Set type and implementing toList and fromList is enough
   for a passing mark of 40%, as long as both toList and fromList satisfy the
   toFromListProp function.

   The maximum mark for those who use Haskell lists to represent a Set is 70%.
   To achieve a higher grade than is, one must write a more efficient
   implementation. 100% is reserved for those brave few who write their own
   self-balancing binary tree.
-}
