# Project Title

Team members:

- Yiqing Li
- Haoyu Liu

## Summary Description

We built a wordle solver, which could help player to solve a wordle game by calculating and comparing entropy of possible answers.

## Project Execution Summary

The program contains four parts:
- Words: all the possible answers and allowed words
- Wordle: main logic of the wordle game
- Entropy: calculate the entropy for the suggested answer
- Main: the IO part

## Additional Details

- List any additional Haskell libraries required for the project: containers, text, pqueue
- For the entropy part, it's easy to calculate entropy of the answer list because of Haskell's declarative feature, using just fold and map
- Were any parts of the code particularly difficult to expres using Haskell?
  What are the challenges in refining and/or refactoring this code to be a
  better example of idiomatic Haskell?

  Generate response (given a guess word and an answer word, produces a response) was tricky because of repeated letters. For example, if answer word is "light", guess word is "hello", only the first "l" in "hello" would be labeled as "1", which means it exists in the word, but wrong position. To solve this, we used wordToMap function, which will convert a word to (letter, set of positions) and check intersection or difference between guess set and answer set.

- Describe any approaches attempted and then abandoned and the reasons why. What
  did you learn by undertaking this project?

  One of the main approaches abandoned is an alternative way to represent the state of the game. Initially we attempted to model the state as the information we have on each letter, e.g., "we have no information on 'a', we know 'b' can't be in index 2, etc". This approach lead to major complications when it comes to updating the state and shrinking the available word list and other features. It was chosen partially because it seemed more obvious, and partially because we wasn't sure of the exact algorithm we were going to use to suggest an answer.

  There were also details in the algorithm that were abandoned. We attempted to compute entropy for the entire allowed word list, and we wanted to display more than just the best word to guess. These lead to performance that was unacceptable for an interactive environment. 

  The major lesson learned here was that we should have gained a better understanding of our problem and a clearer overall picture of the various pieces of our program before designing the specifics. In addition, we should make a rough estimation of the performance of our approach when designing our algorithm。
