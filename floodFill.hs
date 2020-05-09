import Data.List
import Data.Array





toSimpleArray :: Array (Int, Int) a -> [[a]]
toSimpleArray grid = [[grid ! (x, y) | x<-[lowx..highx]] |  y<-[lowy..highy]] 
  where ((lowx, lowy), (highx, highy)) =  bounds grid

createInputArray :: [[a]] -> Array(Int, Int) a
createInputArray grid = array ((0,0),((length $ grid !! 0) - 1,(length grid) - 1))  entries  
  where entries = concatMap (\z -> map (\y -> ((fst y, fst z), snd y))  (snd z)) $ zip [0..] $ map (\x -> zip [0..] x) grid

addSpaces :: [Char] -> [Char]
addSpaces inString = inString ++ replicate (5 - length inString) '.'

inBounds :: Array (Int, Int) [Char] -> (Int, Int) -> Bool
inBounds grid (x, y) = x >= lowx && x <= highx && y >= lowy && y <= highy
  where ((lowx, lowy), (highx, highy)) =  bounds grid

replace :: Array (Int, Int) [Char] -> (Int, Int) -> [Char] -> Array (Int, Int) [Char]
replace grid point replacement = if inBounds grid point then grid // [(point, replacement)] else grid



grid :: Int -> a -> Array(Int, Int) a
grid size value = array ((0,0),(size-1,size-1)) [((x,y),value) | x<-[0..size-1], y<-[0..size-1]]

floodFill :: Array (Int, Int) [Char] ->  (Int, Int) -> [Char] -> [Char] -> Array (Int, Int) [Char]
floodFill grid point@(x, y) target replacement =
  if((not $ inBounds grid point) ||  grid ! (x,y) /= target) then grid 
  else 
    gridNorth
    where grid' = replace grid point replacement
          gridEast = floodFill grid' (x+1, y) target replacement
          gridWest = floodFill gridEast (x-1, y) target replacement
          gridSouth = floodFill gridWest (x, y+1) target replacement
          gridNorth = floodFill gridSouth (x, y-1) target replacement
		  
printGrid :: Show a => Array (Int, Int) a ->  IO [()]
printGrid =  mapM (putStrLn . intercalate " " . map show) . toSimpleArray

main = do
	printGrid $ floodFill (createInputArray [["White", "White", "White", "Blue", "Blue"], ["Blue", "White", "Blue", "Blue", "Blue"], ["Blue", "Blue", "Blue", "Green", "Green"], ["Green", "Red", "Blue", "Black", "Black"], ["Blue", "Blue", "Blue", "Green", "Blue"]]) (1,2) "Blue" "Red"