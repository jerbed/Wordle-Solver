module Wordle where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Words

-- If we don't have information on a letter, it's represented by Null.
-- Else, a letter is either in the answer word or not. If it's in the word,
-- we record the indices that we _know_ is in, and those indices that
-- we _know_ it is not in.
data LetterKnowledge = Null | NotInAnswer | InAnswer (Set.Set Int) (Set.Set Int)
  deriving (Show)

-- The state of the game.
-- For each of the 26 letters, what knowledge do we have on whether they
-- are in the answer & what indices they may be in.
newtype State = GameKnowledge (Map.Map Char LetterKnowledge)
  deriving (Show)

initState :: State
initState = GameKnowledge (Map.fromList (List.map f ['a' .. 'z']))
  where
    f c = (c, Null)

-- data LetterResponse = "0" | 1 | 2

-- instance Show LetterResponse where
--   show Gray = "0"
--   show Yellow = "1"
--   show Green = "2"



-- Input: Original state, guess word, reponse to the guess word
-- Output: the new state.
stateTransition :: State -> String -> [Int] -> State
stateTransition s guess response = 

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
