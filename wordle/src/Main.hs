module Main where

import qualified Data.Map as Map
import qualified Data.Text as Text
import Wordle
import Words (WordleWord, allowedWords, possibleAnswers)

-- import TheGame

-- initialization, fileName: xxx.txt
-- TODO: We can get words from a .txt file, which makes maintenance and updateeasier
-- wordsFromTxt :: String -> IO [String]
-- wordsFromTxt fileName = fmap Text.lines (Text.readFile fileName)

-- if an input word is 5-letter long
validLength :: String -> Bool
validLength x = length x == 5

-- !!! The input word doesn't need to be in the possible answers list.
-- It just needs to be a valid word.
validWord :: String -> Bool
validWord x = x `elem` allowedWords

-- combine validLength and validWord
validInput :: String -> Bool
validInput x = validLength x && validWord x

-- TODO: Should input be a list of possible answers or a history? Are these equivalent?
--
playTheGame :: Int -> State -> [WordleWord] -> IO ()
playTheGame roundNum currState currAnsList =
  if roundNum > 6
    then putStrLn "Game over, failed to find answer"
    else do
      putStr "Begin round "
      print roundNum
      putStrLn "Enter guess" -- !!! TODO: modify this so that word is suggested by the program instead
      inputWord <- getLine
      putStrLn "What was the response? For example, for [Gray, Gray, Yellow, Green, Gray], enter 00120"
      inputResponse <- getLine
      -- TODO:
      if validInput inputWord
        then do
          let (newState, newAnsList) = updateAnsList currState inputWord inputResponse currAnsList
          putStrLn "updated Answer list is (words outside of this list cannot be an answer, but can be guessed as long as it is in the allowedWords list"
          print newAnsList

          playTheGame (roundNum + 1) newState newAnsList
        else do
          putStrLn "Invalid input, please try again:"
          playTheGame roundNum currState currAnsList

main :: IO ()
main = do
  playTheGame 1 (HistoryM Map.empty) initAnsList
  return ()