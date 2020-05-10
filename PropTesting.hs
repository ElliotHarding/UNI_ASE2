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
		x <- choose (0, 25)
		y <- choose (0, 25)
		return (Position x y)

-- Format array for input into floodFilling algorithm
-- Creates a 2d like array of (x y positions) and (colors)
makePositionMap :: [[a]] -> Array(Int, Int) a
makePositionMap colorArray = array ((0,0),((length $ colorArray !! 0) - 1,(length colorArray) - 1)) inData  
  where inData = concatMap (\z -> map (\y -> ((fst y, fst z), snd y)) (snd z)) $ zip [0..] $ map (\x -> zip [0..] x) colorArray

-- No reason in testing the createInputArray function here since its the same as the makePositionMap function used for the property tests
-- It is however tested in the unit tests! Therefore verifying (to an extent) the function makePositionMap used for property tests
	

test_createPrintArray :: ColorMap -> Bool
test_createPrintArray (ColorMap map dimen) = 
	(do 
		let createdArr = createPrintArray $ makePositionMap map
		createdArr == map
	)
	
test_isInsideArray :: ColorMap -> Position -> Bool
test_isInsideArray (ColorMap map dimen) (Position posX posY) =
	(do
		let arr = makePositionMap map
		isInsideArray arr (posX,posY)		
	)
	
test_replace :: ColorMap -> Position -> Bool
test_replace (ColorMap map dimen) (Position posX posY) = 
	(do 
	let arr = makePositionMap map
	let newArr = replace arr (posX,posY) 'r'
	if posX > 0 && posX <= dimen && posY > 0 && posY <= dimen then
		arr /= newArr
	else
		True
	)
	
test_runAlgorithm :: ColorMap -> Position -> Bool
test_runAlgorithm (ColorMap map dimen) (Position posX posY) =  
	(do 
	let posMap = makePositionMap map
	let colToReplace = posMap ! (xPos,yPos)
	let newMap = runAlgorithm map posX posY colToReplace 'o' --Chose a color thats not in random gen to avoid chance of filling same color
	if posX > 0 && posX <= dimen && posY > 0 && posY <= dimen then --For dimension testing this occasionally goes out of bounds therefore this was added 
		map /= newMap
	else
		True	
	)


-- For generating a 3 x 3 map where we know the colors
data ColorMap = ColorListGen Char Char Char Char Char Char Char Char Char deriving (Show, Read)
instance Arbitrary ColorListGen where
    arbitrary = do
	   a <- elements ['r', 'b', 'w', 'g']
	   b <- elements ['r', 'b', 'w', 'g']
	   c <- elements ['r', 'b', 'w', 'g']
	   d <- elements ['r', 'b', 'w', 'g']
	   e <- elements ['r', 'b', 'w', 'g']
	   f <- elements ['r', 'b', 'w', 'g']
	   g <- elements ['r', 'b', 'w', 'g']
	   h <- elements ['r', 'b', 'w', 'g']
	   i <- elements ['r', 'b', 'w', 'g']
	   return (ColorMap a b c d e f g h i)

-- Turns color list gen into 3x3 map
colorListTo3Map :: ColorListGen -> [[Char]]
colorListTo3Map (ColorListGen a b c d e f g h i) = [[a,b,c],[d,e,f],[g,h,i]]

test_runAlgorithm :: ColorListGen -> Position -> Bool
test_runAlgorithm (ColorListGen a b c d e f g h i) (Position posX posY) =  
	(do 
	let map = colorListTo3Map (ColorListGen a b c d e f g h i)
	let newMap = runAlgorithm map posX posY a 'o' --Chose a color thats not in random gen to avoid chance of filling same color
	if posX > 0 && posX <= 2 && posY > 0 && posY <= 2 then
		map /= newMap
	else
		True	
	)
	


-- Functions for turning ColorMap into something usable by functions
-- createMap :: ColorMap -> [[Char]]
-- createMap (ColorMap a b c d e f g h i) = [[a,b,c],[d,e,f],[g,h,i]]

-- createPositionMap :: ColorMap -> Array (Int, Int) Char
-- createPositionMap (ColorMap a b c d e f g h i) = array ((0,0),(2,2)) [((0,0),a),((1,0),b),((2,0),c),((0,1),d),((1,1),e),((2,1),f),((0,2),g),((1,2),h),((2,2),i)]

-- test_createInputArray :: ColorMap -> Bool
-- test_createInputArray map = 
	-- (do
		-- let arr1 = createPositionMap map
		-- let arr2 = createInputArray $ createMap map
		-- arr1 == arr2
	-- )
	
-- test_createPrintArray :: ColorMap -> Bool
-- test_createPrintArray map = 
	-- (do 
		-- let createdArr = createPrintArray $ createPositionMap map
		-- createdArr == createMap map
	-- )
	
-- test_isInsideArray :: ColorMap -> Position -> Bool
-- test_isInsideArray map (Position posX posY) =
	-- (do
		-- let arr = createPositionMap map
		-- let b = isInsideArray arr (posX,posY)
		-- b
	-- )
	
-- test_replace :: ColorMap -> Position -> Bool
-- test_replace map (Position posX posY) = 
	-- (do 
	-- let arr = createPositionMap map
	-- let newArr = replace arr (posX,posY) 'r'
	-- if posX > 0 && posX <= 2 && posY > 0 && posY <= 2 then
		-- arr /= newArr
	-- else
		-- True
	-- )
	
-- test_runAlgorithm :: ColorMap -> Position -> Bool
-- test_runAlgorithm (ColorMap a b c d e f g h i) (Position posX posY) =  
	-- (do 
	-- let map = createMap (ColorMap a b c d e f g h i)
	-- let newMap = runAlgorithm map posX posY a 'o' --Chose a color thats not in random gen to avoid chance of filling same color
	-- if posX > 0 && posX <= 2 && posY > 0 && posY <= 2 then
		-- map /= newMap
	-- else
		-- True	
	-- )

main = do
	--quickCheck test_createInputArray
	quickCheck test_createPrintArray
	--quickCheck test_isInsideArray
	--quickCheck test_replace
	--quickCheck test_runAlgorithm