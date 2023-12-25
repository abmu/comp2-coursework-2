import Test.QuickCheck
import Coursework
import qualified Data.Set as HS

-- intersectionProp :: IO ()
-- intersectionProp =
  -- quickCheck
    -- ((\xs ys -> toList (intersection (fromList xs) (fromList ys)) == HS.toList (HS.intersection (HS.fromList xs) (HS.fromList ys))) :: [Int] -> [Int] -> Bool)

main :: IO ()
main = do
    putStrLn "Running tests for Coursework module which implements a set"
    putStr "toFromListProp"
    toFromListProp
    putStr "eqProp"
    eqProp