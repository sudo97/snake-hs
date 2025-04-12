# Haskell Snake (And not only Snake) Game

A simple Snake game implemented in Haskell using the Brick library for terminal-based UI. And as a bonus - Tetris!

## Overview

This project is a classic Snake game where the player controls a snake that moves around the screen, eating food to grow longer while avoiding collisions with itself. The game features:

- Terminal-based graphics using the Brick library
- Arrow key controls for snake movement
- Random food placement
- Collision detection
- Wrapping around screen edges

Because implementing Snake was so much fun, I decided to add Tetris as well! After all, programming shouldn't be about "endomorphic pseudofunctors" but about writing code that makes us feel happy and productive.

## How to Play

### Snake
- Use arrow keys to change the snake's direction
- Eat food (marked as 'o') to grow longer
- Avoid colliding with your own tail
- Press 'q' or Esc to quit the game

### Tetris
- Use arrow keys to move and rotate pieces
- Press 'q' or Esc to quit the game

## Implementation Details

The game is built with a clear separation of concerns:
- `Game.Core`: Core game logic and state management
- `Game.UI`: User interface and event handling

This project was developed as a recreational activity and as a Test-Driven Development (TDD) exercise, with comprehensive test cases ensuring the game logic works as expected.

And readme was generated by Claude.

## Run the game

By default, the game runs Snake:

```bash
cabal run
```

You can explicitly choose which game to run:

```bash
cabal run -- snake    # Run Snake game
cabal run -- tetris   # Run Tetris game
```

## Run the tests

```bash
cabal test
```

