import Control.Exception (IOException, catch)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import Text.Printf (printf)

directionToInt :: String -> Int
directionToInt (x : xs)
  | x == 'R' = read xs
  | x == 'L' = -(read xs)

-- takes start num and list and counts how many times the total is 0
countZeros :: Int -> [Int] -> Int
countZeros curr [] = 0
countZeros curr (x : xs)
  | ((curr + x) `mod` 100) == 0 = 1 + countZeros ((curr + x) `mod` 100) xs
  | otherwise = countZeros ((curr + x) `mod` 100) xs

main :: IO ()
main =
  do
    args <- getArgs
    case args of
      [] -> putStrLn "Usage: runhaskell solver_template.hs <input_filename>"
      (inputFilename : _) -> do
        startTime <- getCPUTime

        -- Read input file line by line
        content <- readFile inputFilename
        let linesOfInput = lines content

        let signedDirections = map directionToInt linesOfInput

        let solution = countZeros 50 signedDirections

        endTime <- getCPUTime
        let elapsedSeconds = fromIntegral (endTime - startTime) / 1e12 :: Double

        print solution
        putStrLn $ printf "%.6fs" elapsedSeconds
    `catch` handleIOError
  where
    handleIOError :: IOException -> IO ()
    handleIOError e = putStrLn $ "Error reading file: " ++ show e
