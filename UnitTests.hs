import Test.HUnit
import FloodFill

foo :: Int -> (Int, Int)
foo x = (1, x)

partA :: Int -> IO (Int, Int)
partA v = return (v+2, v+3)

partB :: Int -> IO Bool
partB v = return (v > 5)

test1 :: Test
test1 = TestCase (assertEqual "for (foo 3)," (1,2) (foo 3))

test2 :: Test
test2 = TestCase (do (x,y) <- partA 3
                     assertEqual "for the first result of partA," 5 x
                     b <- partB y
                     assertBool ("(partB " ++ show y ++ ") failed") b)
					 
test3 :: Test
test3 = TestCase 
	(do 
	let a = runAlgorithm [['w', 'w', 'w', 'w', 'w'], ['g', 'g', 'w', 'g', 'g'], ['g', 'g', 'w', 'g', 'g'], ['g', 'g', 'w', 'g', 'g'], ['w', 'w', 'w', 'w', 'w']] 0 0 'w' 'r'
	let b = [['r', 'r', 'r', 'r', 'r'], ['g', 'g', 'r', 'g', 'g'], ['g', 'g', 'r', 'g', 'g'], ['g', 'g', 'r', 'g', 'g'], ['r', 'r', 'r', 'r', 'r']]
	assertEqual "Test 3" a b
	)

tests :: Test
tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3]

tests' :: Test
tests' = test [ "test1" ~: "(foo 3)" ~: (1,2) ~=? (foo 3),
                "test2" ~: do (x, y) <- partA 3
                              assertEqual "for the first result of partA," 5 x
                              partB y @? "(partB " ++ show y ++ ") failed" ]

main = runTestTT tests