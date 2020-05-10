module FloodFill where
import Data.List
import Data.Array

-- Check input x and y location point is will be in array
isInsideArray :: Array (Int, Int) Char -> (Int, Int) -> Bool
isInsideArray colorArray (xPos, yPos) = xPos > -1 && yPos > -1 && xPos <= maxX && yPos <= maxY
	-- Get the bounds of colorArray and check them against xPos and yPos in above statement (using where)
	-- bounds function returns bounds of colorArray
	where ((minX, minY), (maxX, maxY)) = bounds colorArray 

-- Replace one color for another in given position in colorArray
replace :: Array (Int, Int) Char -> (Int, Int) -> Char -> Array (Int, Int) Char
replace colorArray (xPos, yPos) newColor = 
	if isInsideArray colorArray (xPos, yPos) then 
		colorArray // [((xPos, yPos), newColor)]
	else 
		colorArray

-- FloodFill recursive function
floodFill :: Array (Int, Int) Char ->  (Int, Int) -> Char -> Char -> Array (Int, Int) Char
floodFill colorArray (xPos, yPos) oldCol newCol =
  -- Check if position of floodSourcePoint is actually in the bounds of the colorArray,
  -- or if the color to be replaced is not the old color
  -- if so exit function since we've gone enough
  if((not(isInsideArray colorArray (xPos,yPos))) ||  colorArray ! (xPos,yPos) /= oldCol) then
	colorArray 

  -- Otherwise we need replace current location and to check all four directions with the floodFill function
  else 
    upColorArray
    where colorArray' = replace colorArray (xPos,yPos) newCol
          rightColorArray = floodFill colorArray' (xPos+1, yPos) oldCol newCol
          leftColorArray = floodFill rightColorArray (xPos-1, yPos) oldCol newCol
          downColorArray = floodFill leftColorArray (xPos, yPos+1) oldCol newCol
          upColorArray = floodFill downColorArray (xPos, yPos-1) oldCol newCol
		  
-- Format array for input into floodFilling algorithm
-- Creates a 2d like array of (x y positions) and (colors)
createInputArray :: [[a]] -> Array(Int, Int) a
createInputArray colorArray = array ((0,0),((length $ colorArray !! 0) - 1,(length colorArray) - 1)) inData  
  where inData = concatMap (\z -> map (\y -> ((fst y, fst z), snd y)) (snd z)) $ zip [0..] $ map (\x -> zip [0..] x) colorArray

-- Format array for printing
-- Shed the position data of the 2d like array so just left with letters of colors
createPrintArray :: Array (Int, Int) a -> [[a]]
createPrintArray colArray = [[colArray ! (x, y) | x<-[minX..maxX]] | y<-[minY..maxY]] 
-- using bounds vars map the chars of the input array into another array
  where ((minX, minY), (maxX, maxY)) = bounds colArray

--Run the flood fill algorithm and print results, provide map, posX, posY, oldCol, newCol -> outputs to console
runAlgorithmPrint :: [[Char]] -> Int -> Int -> Char -> Char -> IO()
runAlgorithmPrint inArray posX posY oldCol newCol = do
	mapM_ putStrLn $ runAlgorithm inArray posX posY oldCol newCol

--Run the flood fill algorithm, provide map, posX, posY, oldCol, newCol -> outputs array of results in char format
runAlgorithm :: [[Char]] -> Int -> Int -> Char -> Char -> [[Char]]
runAlgorithm inArray posX posY oldCol newCol = do
	createPrintArray $ floodFill (createInputArray inArray) (posX,posY) oldCol newCol

--main = do
	--runAlgorithmPrint [['w', 'w', 'w', 'b', 'b'], ['b', 'w', 'b', 'b', 'b'], ['b', 'b', 'b', 'g', 'g'], ['g', 'r', 'b', 'o', 'o'], ['b', 'b', 'b', 'g', 'b']] 1 2 'b' 'r'