import Control.Exception (IOException, catch)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import Text.Printf (printf)

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

        -- Solve
        let solution = 1
        endTime <- getCPUTime
        let elapsedSeconds = fromIntegral (endTime - startTime) / 1e12 :: Double

        print solution
        putStrLn $ printf "%.4fs" elapsedSeconds
    `catch` handleIOError
  where
    handleIOError :: IOException -> IO ()
    handleIOError e = putStrLn $ "Error reading file: " ++ show e
