--module Teesting where

import Test.QuickCheck

prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (xs++ys) == reverse ys ++ reverse xs

split c [] = []
split c xs = xs' : if null xs'' then [] else split c (tail xs'')
    where xs' = takeWhile (/=c) xs
          xs''= dropWhile (/=c) xs

-- show
examples = [('@',"pbv@dcc.fc.up.pt"), ('/',"/usr/include")]

test (c,xs) = unwords ["split", show c, show xs, "=", show ys]
    where ys = split c xs

main = mapM_ (putStrLn.test) examples
-- /show

--main = quickCheck prop_revapp