import Data.Maybe (isNothing, fromJust)

split :: (Ord a) => [a] -> ([a], [a])
split [] = ([],[])
split [x] = ([x], [])
split x = ((take halfIndex x), (drop halfIndex x))
  where myLength = length x
        halfIndex = myLength `div` 2

binarySearch :: (Ord a) => a -> [a] -> Maybe Int
binarySearch x [] = Nothing
binarySearch x [y] 
  | x == y = Just 1
  | otherwise = Nothing
binarySearch x ys
  | x < centerValue = binarySearch x (fst splitted)
  | x >= centerValue = let recurse = binarySearch x (snd splitted) 
                         in if (isNothing recurse) 
                            then Nothing 
                            else (Just (centerValueIndex + (fromJust recurse)))
    where splitted = split ys
          centerValue = head (snd splitted)
          centerValueIndex = (length ys) `div` 2

main :: IO()
main = print(binarySearch 8 [5,10,7,4,5,8,9,10])