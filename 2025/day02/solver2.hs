import Control.Exception (IOException, catch)
import Data.List.Split (splitOn)
import Data.Set qualified as Set
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import Text.Printf (printf)

-- Parse a single substring range into int tuple
parseRange :: String -> (Int, Int)
parseRange s =
  let (start, '-' : end) = span (/= '-') s
   in (read start, read end)

nDigits :: Int -> Int
nDigits i = length (show i)

repeatNumber :: Int -> Int -> Int
repeatNumber base n =
  let ndigits = nDigits base
      powers = [10 ^ (ndigits * i) | i <- [0 .. n - 1]]
   in sum (map (* base) powers)

generateInvalids :: [Int]
generateInvalids =
  [ repeatNumber base times
    | digits <- [2 .. 10],
      let factorDigits = filter (\d -> digits `mod` d == 0) [1 .. digits - 1],
      subNumDigits <- factorDigits,
      let times = digits `div` subNumDigits,
      let minBase = 10 ^ (subNumDigits - 1),
      let maxBase = minBase * 10,
      base <- [minBase .. maxBase - 1]
  ]

main :: IO ()
main =
  do
    args <- getArgs
    case args of
      [] -> putStrLn "Usage: runhaskell solver_template.hs <input_filename>"
      (inputFilename : _) -> do
        -- Read input file line by line
        content <- readFile inputFilename
        let line = head (lines content)
        let strRanges = splitOn "," line
        let ranges = map parseRange strRanges

        startTime <- getCPUTime
        let invalidSet = Set.fromList generateInvalids
        let solution =
              sum
                [ i
                  | (s, e) <- ranges,
                    i <- [s .. e],
                    Set.member i invalidSet
                ]

        endTime <- getCPUTime
        let elapsedSeconds = fromIntegral (endTime - startTime) / 1e12 :: Double

        print solution
        putStrLn $ printf "%.8fs" elapsedSeconds
    `catch` handleIOError
  where
    handleIOError :: IOException -> IO ()
    handleIOError e = putStrLn $ "Error reading file: " ++ show e
