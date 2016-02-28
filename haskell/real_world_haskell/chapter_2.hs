import Data.List

lengthOfList :: [a] -> Int
lengthOfList = foldl (\c _ -> c+1) 0

averageOfList x = realToFrac (sum x) / genericLength x

makePallindrome x = x ++ (reverse x)

checkIfPallindrome x = x == (reverse x)

compareListLength :: [a] -> [b] -> Ordering
compareListLength x y
  | length x < length y = LT
  | length x > length y = GT
  | otherwise           = EQ
sortBySubListLength =  sortBy compareListLength

myIntersperse :: (Show a) => String -> [a] -> String
myIntersperse separator x = foldl (\x y -> (show x) ++ (show y))