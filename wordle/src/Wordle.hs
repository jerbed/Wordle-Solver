module Wordle where

import qualified Data.List as List
import qualified Data.Map as Map
-- import qualified Data.PSQueue as Q -- Delete
import qualified Data.Set as Set
import Words

-- The state is a history of guesses and responses
newtype State = HistoryM (Map.Map Guess Response)
  deriving (Show)

-- TODO: modify this type?
-- a response is a string of length 5.
-- Gray: 0; Yellow: 1; Green: 2
type Response = [Char]

-- A guess; five-letter word
type Guess = String

currState :: State
currState = HistoryM Map.empty

-- TODO: change this to
-- No answer words have been filtered out initially.
initAnsList :: [String]
initAnsList = possibleAnswers

-- wordToMap "hello": [('e',fromList [1]),('h',fromList [0]),('l',fromList [2,3]),('o',fromList [4])]
-- Convert a word to a representation of letter-to-indices map.
wordToMap :: String -> Map.Map Char (Set.Set Int)
wordToMap word = theMap
  where
    (_, theMap) = foldr f (4, Map.empty) word

    f :: Char -> (Int, Map.Map Char (Set.Set Int)) -> (Int, Map.Map Char (Set.Set Int))
    f c (ind, aMap) = case Map.lookup c aMap of
      Nothing -> (ind - 1, Map.insert c (Set.fromList [ind]) aMap)
      Just set -> (ind - 1, Map.adjust (Set.insert ind) c aMap)

-- Input: original response string, char and indices to change to
modifyResponseStr :: Response -> Char -> Set.Set Int -> Response
modifyResponseStr r c =
  foldr
    ( \index tempR ->
        snd $
          foldr
            ( \char (ind, str) ->
                if ind == index
                  then (ind - 1, c : str)
                  else (ind - 1, char : str)
            )
            (4, [])
            tempR
    )
    r

-- For a guess and a (potentially hypothetical) answer, generate the color of response.
-- Guessing "otter" to "stale" should produce "02010", as the second 't' matches nothing.
-- "otter" "treat" -> "01111"
-- "oottt" "state" -> "00120"
generateResponse :: Guess -> String -> Response
generateResponse gWord ansWord = snd (Map.foldrWithKey f (wordToMap ansWord, "nnnnn") (wordToMap gWord))
  where
    -- char and indices are from gWord; map is from ansWord
    f char indices (map, respColor) = case Map.lookup char map of
      Nothing -> (map, modifyResponseStr respColor '0' indices)
      Just ansIndices -> (map, modIndicesAndResponse indices ansIndices respColor)
    -- For guessing "oottt" to answer "state", match the second to last 't' first,...
    modIndicesAndResponse gIndices ansIndices resp =
      let ind2 = gIndices `Set.intersection` ansIndices
          resp2 = modifyResponseStr resp '2' ind2
          gIndices2 = gIndices `Set.difference` ind2
          ansIndices2 = ansIndices `Set.difference` ind2
          indList = Set.toList gIndices2
          ind1 = take (length ansIndices2) indList
          ind0 = drop (length ansIndices2) indList

          resp1 = modifyResponseStr resp2 '1' (Set.fromList ind1)
          resp0 = modifyResponseStr resp1 '0' (Set.fromList ind0)
       in resp0

-- Update the list of words that remain possible answers. They are the words
-- that generate the given Response if they are the actual answer.
-- updateAnsList (HistoryM Map.empty) "peach" "00102" initAnsList = (newState, ["marsh","laugh","rajah","harsh","faith"])
updateAnsList :: State -> Guess -> Response -> [String] -> (State, [String])
updateAnsList (HistoryM m) gWord rColor oldAnsList = (HistoryM (Map.insert gWord rColor m), newAnsList)
  where
    newAnsList = filter f oldAnsList
    f word = generateResponse gWord word == rColor

{- Finding the best guess -}

-- Weight given to words that are answers during entropy calculation.
-- Determined arbitrarily.
weightAns :: Double
weightAns = 9.0

weightNonAns :: Double
weightNonAns = 0.5
