import System.Random
import Data.Array.IArray	

main = do
  g <- newStdGen
  let img = listArray ((0,0),(5,5)) ( myRands g ) :: Array (Int,Int) Int
  putStrLn $ printArray img

myRands :: RandomGen g => g -> [Int]
myRands g = randomRs (0,1) g

make2dRange :: Int -> Int -> Int -> Int -> [(Int,Int)]
make2dRange x1 y1 x2 y2 = range ((x1,y1),(x2,y2))

printArray :: ( Array (Int,Int) Int ) -> String
printArray arr = unlines rows
  where rows = map (unwords . map (show . (!) arr)) rowIndices 
        rowIndices = map ( \y -> make2dRange 0 y 5 y ) [0..5]