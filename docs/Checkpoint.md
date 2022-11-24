# Project Title

Team members:

- Haoyu Liu
- Yiqing Li

## Summary Description

In this project, we are going to build and test a solver for the popular word game  Wordle. Our Haskell program will play the game as a human player would, and our goal is for the program to guess the correct word in as few steps as possible.


## Checkpoint Progress Summary

The whole project is divided into three parts: 
- Words, which contains all the possible words *(**TODO:** Maybe it's possible to replace this part with reading from .txt files)*
- Main, which does the IO interactions with players
- TheGame, which runs the logic of this game

Currently, we have defined most of the types we need, except for the State type, which is still a little confusing, the goal of the State type is to get rid of impossible words, we have two options: 
- Keep the remaining possible words inside the State type as an attribute, so after every step, we have all the words to choose from.
- Only keep the words we guessed and corresponding response from the player, so before every step, we need to compute and get the reamining words list.

Due to the time, we only finished a few functions and data types.

## Additional Details

- List any additional Haskell libraries required for the project (i.e., what
  `extra-deps` have been added to `stack.yaml` files and `[build-depends]` have
  been added to `package.yaml`/`<package_name>.cabal` files).
- Briefly describe the structure of the code (what are the main components, the
  module dependency structure).
- Pose any questions that you may have about your project and/or request
  feedback on specific aspects of the project.

Note: Be sure that all `.hs` source files and any supporting files (e.g.,
`stack.yaml`, `package.yaml`/`<package_name>.cabal` files, data files, examples,
...) have been committed and pushed.
