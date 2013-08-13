-- Parallel Haskell version of the Kaggle Digit Recognizer
-- http://www.kaggle.com/c/digit-recognizer

-- % ghc --make digits.hs -O2 -threaded
-- [1 of 1] Compiling Main             ( digits.hs, digits.o )
-- Linking digits ...
-- % time ./digits ~/tmp/digits/trainingsample.csv <~/tmp/digits/validationsample.csv +RTS -N3
-- 0.944
-- 479.86s user 52.15s system 283% cpu 3:07.67 total

import System.Environment (getArgs)
import Data.List (minimumBy)
import Data.Ord (comparing)
import Control.Parallel.Strategies (parListChunk, using, rseq)

type Pixel = Int

data Row = Row { label :: Int
               , pixels :: [Pixel] }
         deriving Show

type Model = [Row]

main = do
  -- Train on the first arg
  (f:_) <- getArgs
  examples <- readFile f

  -- Classify a validation stream from stdin.  These will be the same
  -- format as the examples, with the correct digit at front.  Later
  -- compare to the guess.
  stdin <- getContents

  -- Create training model
  let model = readCsv $ lines examples

  -- Create candidate list
  let candidates = readCsv $ lines stdin

  -- Build up a list of matches in parallel [0,1,0,0,1,1,1,..]
  let res = map (guess model) candidates `using` (parListChunk 7 rseq)

  let matchCount = fromIntegral $ sum res
  let totalPossible = fromIntegral $ length candidates

  -- Output the accuracy percentage
  putStrLn $ show $ matchCount / totalPossible

guess :: Model -> Row -> Int
guess m candidate =
  if (label guess == label candidate) then 1 else 0
  where guess = classifyRow m (pixels candidate)

readCsv :: [String] -> Model
readCsv xs = map (makeRow . wordsWhen (==',')) $ drop 1 xs

makeRow :: [String] -> Row
makeRow (label:pixels) = 
  Row (read label) (map read pixels)

distance :: [Int] -> [Int] -> Int
distance xs ys =
  foldr (\(x, y) acc -> acc + (x - y)^2) 0 (zip xs ys)

classifyRow :: Model -> [Pixel] -> Row
classifyRow model ps =
  minimumBy (comparing (foo ps)) model
  where foo ps' ex = distance ps' (pixels ex)

-- http://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell/4981265#4981265
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen f s =
  case dropWhile f s of
    "" -> []
    s' -> w : wordsWhen f s''
      where (w, s'') = break f s'
