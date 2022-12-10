module Wordle where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
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

-- No answer words have been filtered out initially.
initAnsList :: [String]
initAnsList = possibleAnswers

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

-- Outer fold: for each index in set , do thing
-- Inner fold, for that index, modify the string accordingly

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
-- updateAnsList (History Map.empty) "peach" "00102" initAnsList = (newState, ["marsh","laugh","rajah","harsh","faith"])
updateAnsList :: State -> Guess -> Response -> [String] -> (State, [String])
updateAnsList (HistoryM m) gWord rColor oldAnsList = (HistoryM (Map.insert gWord rColor m), newAnsList)
  where
    newAnsList = filter f oldAnsList
    f word = generateResponse gWord word == rColor

---------------------
---------------------
---------------------
---------------------

-- -- If we don't have information on a letter, it's represented by Null.
-- -- Else, a letter is either in the answer word or not. If it's in the word,
-- -- we record the indices that we _know_ is in, and those indices that
-- -- we _know_ it is not in.
-- data LetterKnowledge = Null | NotInAnswer | InAnswer (Set.Set Int) (Set.Set Int)
--   deriving (Show)

-- Representation of a word.
-- The word itself & a map from letters to indices.
-- "otter" (fromList [('e',fromList [3]),('o',fromList [0]),('r',fromList [4]),('t',fromList [1,2])])
-- data WordMap = WordMap String (Map.Map Char (Set.Set Int))
--   deriving (Show)

-- -- convert a 5-letter word to its WordMap representation
-- convertWordToMap :: String -> WordMap
-- convertWordToMap word = WordMap word theMap
--   where
--     (_, theMap) = foldr f (4, Map.empty) word

--     f :: Char -> (Int, Map.Map Char (Set.Set Int)) -> (Int, Map.Map Char (Set.Set Int))
--     f c (ind, aMap) = case Map.lookup c aMap of
--       Nothing -> (ind - 1, Map.insert c (Set.fromList [ind]) aMap)
--       Just set -> (ind - 1, Map.adjust (Set.insert ind) c aMap)

-- -- Map.fromList (List.zip word [0 .. 4])

-- processResponse :: String -> Response -> (Set.Set Int, Set.Set Int)
-- processResponse = undefined

-- -- word colors =

-- -- From indices in word and the response to word to the set of responses(0,1,2)
-- -- Output: Set of responses, e.g., "02", that is, the character is
-- responseSet :: Set.Set Int -> Response -> Set.Set Char
-- responseSet indSet response = theSet
--   where
--     (_, theSet) = foldr (\respChar (ind, set) -> if ind `Set.member` indSet then (ind - 1, Set.insert respChar set) else (ind - 1, set)) (4, Set.empty) response

-- -- Input: Original state, guess word, reponse to the guess word
-- -- Output: Nothing, if the new info conflicts with the existing state,
-- -- else just the new state.
-- stateTransition :: State -> String -> Response -> Maybe State
-- stateTransition (GameKnowledge (Map.Map c letterKnowl)) guess response =
--   let
--     -- wordMap for "otter": [('e',[3]),('o',[0]),('r',[4]),('t',[1,2])]
--     (_, wordMap) = convertWordToMap guess
--     -- response of guessing "otter" to "trace":
--     -- "01011"
--     -- responseMap of guessing "otter" to "trace":
--     -- [(o, [0]),(t, [1,0]), ...]
--     responseMap = foldrWithKey f (Map.empty) wordMap
--     f key indSet tempMap = Map.insert key (responseSet indSet response) tempMap

-- traverse through wordMap, look for response in responseMap,
-- record the char and its repoonse set in a map.

-- newtype Response = Response [LetterResponse]

-- Input: guess word and answer,
-- respondToGuess :: WordleWord -> WordleWord -> Response
-- respondToGuess = undefined
-- respondToGuess guess answer = Response (zipWith convert green_spots (getLetters guess))
--   where
--     answer_letters = lettersInWord answer

--     green_spots :: [Bool]
--     green_spots = zipWith (==) (getLetters guess) (getLetters answer)

--     convert :: Bool -> Char -> LetterResponse
--     convert False the_letter
--       | the_letter `Set.member` answer_letters = Yellow
--       | otherwise = Gray
--     convert True _ = Green

-- advanceState :: State -> WordleWord -> Response -> State
-- advanceState state word response = state <> new_state
--   where
--     -- a new State reflecting only information in the Response
--     new_state = State new_mapping

--     guess_with_response :: [(Char, LetterResponse)]
--     guess_with_response = zip (getLetters word) (getResponses response)

-- new_mapping = ... go mempty guess_with_response
--   where
--     go :: Map.Map Char LetterInfo -> Int -> (Char, LetterResponse) -> Map.Map Char LetterInfo
--     go old_mapping _ (letter, Gray) = Map.insert letter LetterNotInWord old_mapping
--     go old_mapping index (letter, Yellow) = Map.insert letter (LetterNotInLocations (Set.singleton index)) old_mapping
--     go old_mapping index (letter, Green) = Map.insert letter (LetterNotInLocations (Set.delete index (Set.fromList allLocations))) old_mapping

-- The state of the game; can be thought of as
-- representing a human player's knowledge at a particular point
-- in the game, i.e., which letters are not in the answer,
-- which letters are but are not in certain positions.
-- newtype State = State (Map.Map Char LetterInfo)

-- shrinkWordList :: State -> [String] -> [String]
-- shrinkWordList (State m) wl = List.filter p wl
--   where
--     p :: String -> Bool
--     p word = foldl f (0, True) word

--     f :: (Int, Bool) -> Char -> (Int, Bool)
--     f (ind, False) _ = (ind + 1, False)
--     f (ind, True) letter = case Map.lookup letter m of
--       Nothing -> (ind + 1, True)
--       Just LetterAbsent -> (ind + 1, False)
--       Just (LetterWrongIndices wrongIndices) -> (ind + 1, Set.member ind wrongIndices)
