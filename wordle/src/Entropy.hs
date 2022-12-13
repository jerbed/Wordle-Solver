module Entropy where

import qualified Data.List as List
import qualified Data.Map as Map
-- import qualified Data.PSQueue as Q -- Delete
import qualified Data.Set as Set
import Wordle
import Words

data EntropyList = EntropyList [(Double, Guess)] [WordleWord]

-- Also shows whether the word is in possibleAnswers and haven't been eliminated.
instance Show EntropyList where
  show (EntropyList l remainingWordsList) =
    show
      ( fmap
          ( \(entropy, word) -> (entropy, word, word `List.elem` possibleAnswers && word `List.elem` remainingWordsList)
          )
          l
      )

-- Each response pattern and its corresponding words
newtype PatternAndWords = PatternAndWords (Map.Map Response [WordleWord])
  deriving (Show)

-- Whether a word is in the possibleAnswer. Pre-calculate for performance.
inAnsMap :: Map.Map WordleWord Bool
inAnsMap = Map.fromList (fmap (\w -> (w, w `List.elem` possibleAnswers)) allowedWords)

-- GIVEN a guesss word a the list of words to guess, produce the map
-- of response patterns and answers that produce that pattern
calcGuessPatAndWords :: Guess -> [WordleWord] -> PatternAndWords
calcGuessPatAndWords gWord remainingWordsList = PatternAndWords (foldr f Map.empty remainingWordsList)
  where
    f :: String -> Map.Map Response [WordleWord] -> Map.Map Response [WordleWord]
    f word map = if r word `Map.member` map then Map.adjust (word :) (r word) map else Map.insert (r word) [word] map
    r = generateResponse gWord

-- Calculate the entropy of a pattern
calcGuessEntropy :: PatternAndWords -> [WordleWord] -> Double
calcGuessEntropy (PatternAndWords map) remainingWordsList = Map.foldr f 0.0 map
  where
    f wordSublist val = val - prob wordSublist * logBase 2 (prob wordSublist)
    prob wordSublist = calcProb wordSublist / p
    calcProb =
      foldr
        ( \w x ->
            x
              + ( case Map.lookup w inAnsMap of
                    Nothing -> weightNonAns
                    Just bool -> if bool then weightAns else weightNonAns
                )
        )
        0.0
    p = calcProb remainingWordsList

-- Calculate the list of entropy for the entire input word list
calcEntropyList :: [WordleWord] -> EntropyList
calcEntropyList remainingWordsList =
  EntropyList
    ( map
        (\gWord -> (c gWord, gWord))
        allowedWords
    )
    remainingWordsList
  where
    c gWord = calcGuessEntropy (calcGuessPatAndWords gWord remainingWordsList) remainingWordsList

-- Make a guess using a vastly simplified process.
-- Only possible answers are considered.
-- Only shows the word with the highest entropy.
calcBestWordSimplified :: [WordleWord] -> (WordleWord, Double)
calcBestWordSimplified wordList = foldr f ("", 0) wordList
  where
    f word (prevWord, prevEntropy) =
      if entropy word > prevEntropy
        then (word, entropy word)
        else (prevWord, prevEntropy)
    entropy word = calcGuessEntropy (calcGuessPatAndWords word wordList) wordList
