import Data.List
import Data.Array

-- Format array for printing
createPrintArray :: Array (Int, Int) a -> [[a]]
createPrintArray grid = [[grid ! (x, y) | x<-[lowx..highx]] |  y<-[lowy..highy]] 
  where ((lowx, lowy), (highx, highy)) =  bounds grid

-- Format array for input into floodFilling algorithm
createInputArray :: [[a]] -> Array(Int, Int) a
createInputArray grid = array ((0,0),((length $ grid !! 0) - 1,(length grid) - 1))  entries  
  where entries = concatMap (\z -> map (\y -> ((fst y, fst z), snd y))  (snd z)) $ zip [0..] $ map (\x -> zip [0..] x) grid

--addSpaces :: [Char] -> [Char]
--addSpaces inString = inString ++ replicate (5 - length inString) '.' <-- Trying to change output

inBounds :: Array (Int, Int) [Char] -> (Int, Int) -> Bool
inBounds grid (x, y) = x >= lowx && x <= highx && y >= lowy && y <= highy
  where ((lowx, lowy), (highx, highy)) =  bounds grid

replace :: Array (Int, Int) [Char] -> (Int, Int) -> [Char] -> Array (Int, Int) [Char]
replace grid location newColor = if inBounds grid location then grid // [(location, newColor)] else grid

-- Array object used everywhere
grid :: Int -> a -> Array(Int, Int) a
grid size value = array ((0,0),(size-1,size-1)) [((x,y),value) | x<-[0..size-1], y<-[0..size-1]]

floodFill :: Array (Int, Int) [Char] ->  (Int, Int) -> [Char] -> [Char] -> Array (Int, Int) [Char]
floodFill colorArray floodSourcePoint@(xPos, yPos) oldCol newCol =
  -- Check if position of floodSourcePoint is actually in the bounds of the colorArray,
  -- or if the color to be replaced is not the old color
  -- if so exit function since we've gone enough
  if((not(inBounds colorArray floodSourcePoint)) ||  colorArray ! (xPos,yPos) /= oldCol) then colorArray 
  
  -- Otherwise we need to check all four directions with the floodFill function
  else 
    upColorArray
    where colorArray' = replace colorArray floodSourcePoint newCol
          rightColorArray = floodFill colorArray' (xPos+1, yPos) oldCol newCol
          leftColorArray = floodFill rightColorArray (xPos-1, yPos) oldCol newCol
          downColorArray = floodFill leftColorArray (xPos, yPos+1) oldCol newCol
          upColorArray = floodFill downColorArray (xPos, yPos-1) oldCol newCol

printGrid :: Show a => Array (Int, Int) a ->  IO [()]
printGrid =  mapM (putStrLn . intercalate " " . map show) . createPrintArray

main = do
	printGrid $ floodFill (createInputArray [["White", "White", "White", "Blue", "Blue"], ["Blue", "White", "Blue", "Blue", "Blue"], ["Blue", "Blue", "Blue", "Green", "Green"], ["Green", "Red", "Blue", "Black", "Black"], ["Blue", "Blue", "Blue", "Green", "Blue"]]) (1,2) "Blue" "Red"