module Main where

import qualified Data.Text as Text
import Words (possibleAnswers)

-- import TheGame

-- initialization, fileName: xxx.txt
-- TODO: We can get words from a .txt file, which makes maintenance and updateeasier
-- wordsFromTxt :: String -> IO [String]
-- wordsFromTxt fileName = fmap Text.lines (Text.readFile fileName)

-- if an input word is 5-letter long
validLength :: String -> Bool
validLength x = length x == 5

-- if the input word is in the possible answers list
validWord :: String -> Bool
validWord x = x `elem` possibleAnswers

-- combine validLength and validWord
validInput :: String -> Bool
validInput x = validLength x && validWord x

getInput :: IO String
getInput = do
  input <- getLine
  -- TODO:
  if validInput input
    then return "Hello"
    else do
      putStrLn "Invalid input, please try again:"
      getInput

main :: IO ()
main = do
  getInput
  return ()