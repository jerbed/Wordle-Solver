module TheGame where

{-
  This is the the module where the game is working
-}
import qualified Data.Foldable
import Words

data Color = Red | Yellow | Green

instance Show Color where
  show Red = "X"
  show Yellow = "M"
  show Green = "O"

type Response = [Color]

-- every step, we have a Computer guess(word), and a human response
type Step = (WordleWord, Response)

instance Show Step where
  show (word, response) = show word ++ show response

-- current game state is represented by a list of steps
-- TODO: Maybe WordleWords could be contained in the GameState
type GameState = [Step]

nextPossibleWord :: WordleWords -> GameState -> String
nextPossibleWord = undefined
