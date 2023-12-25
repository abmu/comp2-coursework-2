import Test.QuickCheck
import Coursework
import qualified Data.Set as HS

emptyProp :: IO ()
emptyProp
  | toList empty == ([] :: [Int]) = putStrLn "+++ OK, passed 1 test."
  | otherwise = putStrLn "*** Failed!"

nullProp :: IO ()
nullProp =
  quickCheck
    ((\xs -> HS.null (HS.fromList xs) == Coursework.null (fromList xs)) :: [Int] -> Bool)

singletonProp :: IO ()
singletonProp =
  quickCheck
    ((\x -> HS.toList (HS.singleton x) == toList (singleton x)) :: Int -> Bool)

insertProp :: IO ()
insertProp =
  quickCheck
    ((\x xs -> HS.toList (HS.insert x (HS.fromList xs)) == toList (insert x (fromList xs))) :: Int -> [Int] -> Bool)

unionProp :: IO ()
unionProp =
  quickCheck
    ((\xs ys -> HS.toList (HS.union (HS.fromList xs) (HS.fromList ys)) == toList (union (fromList xs) (fromList ys))) :: [Int] -> [Int] -> Bool)

intersectionProp :: IO ()
intersectionProp =
  quickCheck
    ((\xs ys -> HS.toList (HS.intersection (HS.fromList xs) (HS.fromList ys)) == toList (intersection (fromList xs) (fromList ys))) :: [Int] -> [Int] -> Bool)

differenceProp :: IO ()
differenceProp =
  quickCheck
    ((\xs ys -> HS.toList (HS.difference (HS.fromList xs) (HS.fromList ys)) == toList (difference (fromList xs) (fromList ys))) :: [Int] -> [Int] -> Bool)

memberProp :: IO ()
memberProp =
  quickCheck $ withMaxSuccess 10000
    ((\x xs -> HS.member x (HS.fromList xs) == member x (fromList xs)) :: Int -> [Int] -> Bool)

cardinalityProp :: IO ()
cardinalityProp =
  quickCheck
    ((\xs -> HS.size (HS.fromList xs) == cardinality (fromList xs)) :: [Int] -> Bool)

setmapProp :: IO ()
setmapProp = do
  quickCheck
    ((\xs -> HS.toList (HS.map (1 +) (HS.fromList xs)) == toList (setmap (1 +) (fromList xs))) :: [Int] -> Bool)
  putStr "setmapProp func 2    "
  quickCheck
    ((\xs -> HS.toList (HS.map (100 -) (HS.fromList xs)) == toList (setmap (100 -) (fromList xs))) :: [Int] -> Bool)

setfoldrProp :: IO ()
setfoldrProp = do
  quickCheck
    ((\xs -> HS.foldr (+) 0 (HS.fromList xs) == setfoldr (+) (fromList xs) 0) :: [Int] -> Bool)
  putStr "setfoldrProp func 2  "
  quickCheck
    ((\xs -> HS.foldr (-) (-10) (HS.fromList xs) == setfoldr (-) (fromList xs) (-10)) :: [Int] -> Bool)

removeSetProp :: IO ()
removeSetProp =
  quickCheck $ withMaxSuccess 10000
    ((\x xs -> HS.toList (HS.delete x (HS.fromList xs)) == toList (removeSet x (fromList xs))) :: Int -> [Int] -> Bool)

powerSetProp :: IO ()
powerSetProp =
  quickCheck
    ((\xs -> map HS.toList (HS.toList (HS.powerSet (HS.fromList (take 10 xs)))) == map toList (toList (powerSet (fromList (take 10 xs))))) :: [Int] -> Bool)

main :: IO ()
main = do
    putStrLn "Running tests for Coursework module which implements a set"
    putStrLn "=========================================================="
    putStr "toFromListProp       "
    toFromListProp
    putStr "eqProp               "
    eqProp
    putStr "emptyProp            "
    emptyProp
    putStr "nullProp             "
    nullProp
    putStr "singletonProp        "
    singletonProp
    putStr "insertProp           "
    insertProp
    putStr "unionProp            "
    unionProp
    putStr "intersectionProp     "
    intersectionProp
    putStr "differenceProp       "
    differenceProp
    putStr "memberProp           "
    memberProp
    putStr "cardinalityProp      "
    cardinalityProp
    putStr "setmapProp           "
    setmapProp
    putStr "setfoldrProp         "
    setfoldrProp
    putStr "removeSetProp        "
    removeSetProp
    putStr "powerSetProp         "
    powerSetProp