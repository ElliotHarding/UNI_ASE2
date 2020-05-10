import Data.List
import Data.Array
import FloodFill
import Test.QuickCheck

-- 3x3 colour map
data ColorMap = ColorMap [[Char]] Int deriving (Show, Read)
data Position = Position Int Int deriving (Show, Read)

-- Generator for ColorMap
-- Called each time a ColorMap instance is created during prop runtime
instance Arbitrary ColorMap where
    arbitrary = do
		k <- choose (2, 20)
		map <- sequence [ sequence [ elements ['r', 'b', 'w', 'g'] | _ <- [1..k] ] | _ <- [1..k] ]
		return (ColorMap map k)
		
-- Generator for Position
instance Arbitrary Position where
    arbitrary = do
		x <- choose (-5, 25) -- Positions include data outside of the max range of the generated arrays for testing out of range
		y <- choose (-5, 25)
		return (Position x y)

-- Format array for input into floodFilling algorithm
-- Creates a 2d like array of (x y positions) and (colors)
makePositionMap :: [[a]] -> Array(Int, Int) a
makePositionMap colorArray = array ((0,0),((length $ colorArray !! 0) - 1,(length colorArray) - 1)) inData  
  where inData = concatMap (\z -> map (\y -> ((fst y, fst z), snd y)) (snd z)) $ zip [0..] $ map (\x -> zip [0..] x) colorArray

-- No reason in testing the createInputArray function here since its the same as the makePositionMap function used for the property tests
-- It is however tested in the unit tests! Therefore verifying (to an extent) the function makePositionMap used for property tests
	
-- test_createPrintArray
test_createPrintArray :: ColorMap -> Bool
test_createPrintArray (ColorMap map dimen) = 
	(do 
		let createdArr = createPrintArray $ makePositionMap map
		createdArr == map
	)

-- test_isInsideArray
test_isInsideArray :: ColorMap -> Position -> Bool
test_isInsideArray (ColorMap map dimen) (Position posX posY) =
	(do
		let arr = makePositionMap map
		let b = isInsideArray arr (posX,posY)		
		if not b && (posX >= dimen || posY >= dimen || posX < 0 || posY < 0) then
			True
		else
			b				
	)

-- test_replace
test_replace :: ColorMap -> Position -> Bool
test_replace (ColorMap map dimen) (Position posX posY) = 
	(do 
	let arr = makePositionMap map
	let newArr = replace arr (posX,posY) 'o' --A color thats not in random gen to avoid chance of filling same color
	if posX >= 0 && posX < dimen && posY >= 0 && posY < dimen then
		arr /= newArr
	else
		True
	)
	
-- test_runAlgorithm
test_runAlgorithm :: ColorMap -> Position -> Bool
test_runAlgorithm (ColorMap map dimen) (Position posX posY) =  
	(do 
	let posMap = makePositionMap map
	let colToReplace = posMap ! (posX,posY)
	let newMap = runAlgorithm map posX posY colToReplace 'o' --A color thats not in random gen to avoid chance of filling same color
	if posX >= 0 && posX < dimen && posY >= 0 && posY < dimen then --For dimension testing this occasionally goes out of bounds therefore this was added 
		map /= newMap
	else
		True	
	)

main = do
	quickCheck test_createPrintArray
	quickCheck test_isInsideArray
	quickCheck test_replace
	quickCheck test_runAlgorithm