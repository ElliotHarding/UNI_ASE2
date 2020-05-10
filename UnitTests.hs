import Test.HUnit
import FloodFill

-- ----------------------------------
--	Tests for isInsideArray 
-- ----------------------------------

test_isInsideArray_1 :: Test
test_isInsideArray_1 = TestCase
	(do
		let arr = createInputArray [['w', 'w', 'w', 'w', 'w'], ['g', 'g', 'w', 'g', 'g'], ['g', 'g', 'w', 'g', 'g'], ['g', 'g', 'w', 'g', 'g'], ['w', 'w', 'w', 'w', 'w']]
		let b = isInsideArray arr (5,5)
		assertBool "test_isInsideArray_1" (not b)
	)
	
test_isInsideArray_2 :: Test
test_isInsideArray_2 = TestCase
	(do
		let arr = createInputArray [['w', 'w', 'w', 'w', 'w'], ['g', 'g', 'w', 'g', 'g'], ['g', 'g', 'w', 'g', 'g'], ['g', 'g', 'w', 'g', 'g'], ['w', 'w', 'w', 'w', 'w']]
		let b = isInsideArray arr (0,0)
		assertBool "test_isInsideArray_2" b
	)
	
test_isInsideArray_3 :: Test
test_isInsideArray_3 = TestCase
	(do
		let arr = createInputArray [['w', 'w', 'w', 'w', 'w'], ['g', 'g', 'w', 'g', 'g'], ['g', 'g', 'w', 'g', 'g'], ['g', 'g', 'w', 'g', 'g'], ['w', 'w', 'w', 'w', 'w']]
		let b = isInsideArray arr (4,4)
		assertBool "test_isInsideArray_3" b
	)
	
-- ----------------------------------
--	Tests for createPrintArray
-- ----------------------------------
	
test_createPrintArray_1 :: Test
test_createPrintArray_1 = TestCase
	(do 
	let arr = createInputArray [['w', 'w', 'w', 'w', 'w'], ['g', 'g', 'w', 'g', 'g'], ['g', 'g', 'w', 'g', 'g'], ['g', 'g', 'w', 'g', 'g'], ['w', 'w', 'w', 'w', 'w']]
	let createdArr = createPrintArray arr
	assertEqual "test_createPrintArray_1" arr createdArr
	)
	
	
-- ----------------------------------
--	Tests for replace
-- ----------------------------------

test_replace_1 :: Test
test_replace_1 = TestCase
	(do 
	let arr = createInputArray [['w', 'w', 'w', 'w', 'w'], ['g', 'g', 'w', 'g', 'g'], ['g', 'g', 'w', 'g', 'g'], ['g', 'g', 'w', 'g', 'g'], ['w', 'w', 'w', 'w', 'w']]
	let newArr = replace arr (0,0) 'r'
	let suposedResult = createInputArray [['r', 'w', 'w', 'w', 'w'], ['g', 'g', 'w', 'g', 'g'], ['g', 'g', 'w', 'g', 'g'], ['g', 'g', 'w', 'g', 'g'], ['w', 'w', 'w', 'w', 'w']]
	assertEqual "test_replace_1" suposedResult newArr
	)
	
test_replace_2 :: Test
test_replace_2 = TestCase
	(do 
	let arr = createInputArray [['w', 'w', 'w', 'w', 'w'], ['g', 'g', 'w', 'g', 'g'], ['g', 'g', 'w', 'g', 'g'], ['g', 'g', 'w', 'g', 'g'], ['w', 'w', 'w', 'w', 'w']]
	let newArr = replace arr (4,4) 'r'
	let suposedResult = createInputArray [['w', 'w', 'w', 'w', 'w'], ['g', 'g', 'w', 'g', 'g'], ['g', 'g', 'w', 'g', 'g'], ['g', 'g', 'w', 'g', 'g'], ['w', 'w', 'w', 'w', 'r']]
	assertEqual "test_replace_2" suposedResult newArr
	)

test_replace_3 :: Test
test_replace_3 = TestCase
	(do 
	let arr = createInputArray [['w', 'w', 'w', 'w', 'w'], ['g', 'g', 'w', 'g', 'g'], ['g', 'g', 'w', 'g', 'g'], ['g', 'g', 'w', 'g', 'g'], ['w', 'w', 'w', 'w', 'w']]
	let newArr = replace arr (5,5) 'r'	
	assertEqual "test_replace_3" arr newArr
	)
	
-- ----------------------------------
--	Tests for runAlgorithm
--
-- While there is also the printRunAlgorithm function, it cannot be tested due to multi line output, however all it does is format the result from this function
-- ----------------------------------
test_runAlgorithm_1 :: Test
test_runAlgorithm_1 = TestCase 
	(do 
	let a = runAlgorithm [['w', 'w', 'w', 'w', 'w'], ['g', 'g', 'w', 'g', 'g'], ['g', 'g', 'w', 'g', 'g'], ['g', 'g', 'w', 'g', 'g'], ['w', 'w', 'w', 'w', 'w']] 0 0 'w' 'r'
	let b = [['r', 'r', 'r', 'r', 'r'], ['g', 'g', 'r', 'g', 'g'], ['g', 'g', 'r', 'g', 'g'], ['g', 'g', 'r', 'g', 'g'], ['r', 'r', 'r', 'r', 'r']]
	assertEqual "test_runAlgorithm_1" a b
	)
	
test_runAlgorithm_2 :: Test
test_runAlgorithm_2 = TestCase 
	(do 
	let a = runAlgorithm [['w', 'w', 'w', 'b', 'b'], ['b', 'w', 'b', 'b', 'b'], ['b', 'b', 'b', 'g', 'g'], ['g', 'r', 'b', 'o', 'o'], ['b', 'b', 'b', 'g', 'b']] 1 2 'b' 'r'
	let b = [['w', 'w', 'w', 'r', 'r'], ['r', 'w', 'r', 'r', 'r'], ['r', 'r', 'r', 'g', 'g'], ['g', 'r', 'r', 'o', 'o'], ['r', 'r', 'r', 'g', 'b']]
	assertEqual "test_runAlgorithm_2" a b
	)
	
test_runAlgorithm_3 :: Test
test_runAlgorithm_3 = TestCase 
	(do 
	let a = runAlgorithm [['w', 'w', 'w', 'b', 'b'], ['b', 'w', 'b', 'b', 'b'], ['b', 'b', 'b', 'g', 'g'], ['g', 'r', 'b', 'o', 'o'], ['b', 'b', 'b', 'g', 'b']] 5 2 'b' 'r'
	let b = [['w', 'w', 'w', 'b', 'b'], ['b', 'w', 'b', 'b', 'b'], ['b', 'b', 'b', 'g', 'g'], ['g', 'r', 'b', 'o', 'o'], ['b', 'b', 'b', 'g', 'b']]
	assertEqual "test_runAlgorithm_3" a b
	)

tests :: Test
tests = TestList [TestLabel "test_isInsideArray_1" test_isInsideArray_1, TestLabel "test_isInsideArray_2" test_isInsideArray_2, TestLabel "test_isInsideArray_3" test_isInsideArray_3, TestLabel "test_runAlgorithm_1" test_runAlgorithm_1, TestLabel "test_runAlgorithm_2" test_runAlgorithm_2, TestLabel "test_runAlgorithm_3" test_runAlgorithm_3]

main = runTestTT tests