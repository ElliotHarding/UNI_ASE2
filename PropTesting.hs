import Data.List
import Data.Array
import FloodFill
import Test.QuickCheck

-- 3x3 colour map
data ColorMap = ColorMap Char Char Char Char Char Char Char Char Char deriving (Show, Read)
data Position = Position Int Int deriving (Show, Read)

-- Generator for ColorMap
instance Arbitrary ColorMap where
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
		
-- Generator for Position
instance Arbitrary Position where
    arbitrary = do
		x <- choose (0, 4)
		y <- choose (0, 4)
		return (Position x y)

-- Functions for turning ColorMap into something usable by functions
createMap :: ColorMap -> [[Char]]
createMap (ColorMap a b c d e f g h i) = [[a,b,c],[d,e,f],[g,h,i]]

createPositionMap :: ColorMap -> Array (Int, Int) Char
createPositionMap (ColorMap a b c d e f g h i) = array ((0,0),(2,2)) [((0,0),a),((1,0),b),((2,0),c),((0,1),d),((1,1),e),((2,1),f),((0,2),g),((1,2),h),((2,2),i)]

test_createInputArray :: ColorMap -> Bool
test_createInputArray map = 
	(do
		let arr1 = createPositionMap map
		let arr2 = createInputArray $ createMap map
		arr1 == arr2
	)
	
test_createPrintArray :: ColorMap -> Bool
test_createPrintArray map = 
	(do 
		let createdArr = createPrintArray $ createPositionMap map
		createdArr == createMap map
	)
	
test_isInsideArray :: ColorMap -> Position -> Bool
test_isInsideArray map (Position posX posY) =
	(do
		let arr = createPositionMap map
		let b = isInsideArray arr (posX,posY)
		b
	)
	
test_replace :: ColorMap -> Position -> Bool
test_replace map (Position posX posY) = 
	(do 
	let arr = createPositionMap map
	let newArr = replace arr (posX,posY) 'r'
	if posX > 0 && posX <= 2 && posY > 0 && posY <= 2 then
		arr /= newArr
	else
		True
	)
	
test_runAlgorithm :: ColorMap -> Position -> Bool
test_runAlgorithm (ColorMap a b c d e f g h i) (Position posX posY) =  
	(do 
	let map = createMap (ColorMap a b c d e f g h i)
	let newMap = runAlgorithm map posX posY a 'o' --Chose a color thats not in random gen to avoid chance of filling same color
	if posX > 0 && posX <= 2 && posY > 0 && posY <= 2 then
		map /= newMap
	else
		True	
	)

main = do
	quickCheck test_createInputArray
	quickCheck test_createPrintArray
	quickCheck test_isInsideArray
	quickCheck test_replace
	quickCheck test_runAlgorithm