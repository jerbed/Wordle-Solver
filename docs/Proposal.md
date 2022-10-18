# Wordle solver in Haskell

Team members:

- Yiqing Li
- Haoyu Liu


## Summary Description

In this project, we are going to build and test a solver for the popular word game  Wordle. Our Haskell program will play the game as a human player would, and our goal is for the program to guess the correct word in as few steps as possible.

[`About wordle`](https://en.wikipedia.org/wiki/Wordle) : 

Wordle is a game in which player needs to guess a **five-letter** word within **six** tries. After every guess, each letter is marked as either green, yellow or gray: green indicates that letter is correct and in the correct position, yellow means it is in the answer but not in the right position, while gray indicates it is not in the answer at all.



## Additional Details

### Use case
At the beginning, a five-letter word is picked for the computer to guess, either from the actual Wordle game or picked by the user from a set of words. The computer will suggest a list (could be singleton) of words to guess next, then the user should indicate the actual word chosen and the result of the guess: a five-letter string with each character representing green, yellow, or gray, indicating how correct the guess was. Then computer will make another 
  guess, the user should provide the feedback, until either the computer uses up six 
  chances without right solution, which will print "I lost", or the computer makes 
  the right guess, verified by the user. Should the user try to cheat or make 
  fun of the computer (change answer in the middle of the game or indicate wrong 
  result on purpose, which will lead to no solution), it will print "You cheated!"

### Intended components (key functions, key data structures, separate modules).
The main components of our program will be 
- an IO module for accepting and parsing user input and displaying suggested words, 
- a module for keeping track of the state of the game (including the list of words that are still valid given the history of the game play), and transitioning state, e.g., trimming the list of possible words according to the parsed inputs.
- a module for suggesting the next words based on the current state.

Thus the functions and data structures might be
  - key functions: probability: calculate the probability of next word; 
    parseResult: convert from the color provided by the use to 3 different states
  - key data structures: the whole vocabulary, colors in different position, currently valid vocabulary.

### Thoughts on testing.
  - Test if the logic is correct, no matter what the result computer gives,
    "I lost", "I won!", or "You cheated!", make sure the result is representing the 
    right situation.
  - Test the accuracy of guesses: $\frac{number of ("I won!")}{number of ("I won!") + number of ("I lost")}$


### Goals
  - Minimal viable product: the application is able to correctly process input and transition game state, according to some suitably defined game state. The application suggests words to guess based on some simple rules with no guarantee for accuracy.
  - Standard goal: implement a guessing algorithm with reasonable level of accuracy.
  - Stretch goal: Implement [Grant Sanderson's algorithm](https://www.youtube.com/watch?v=v68zYyaEmEA) for solving the game.

### Expected functionality to be completed at the Checkpoint.
  - Five-letter word vocabulary
  - Main structure of the game, including the basic structure of game state transition,  without the heuristic search algorithm.

