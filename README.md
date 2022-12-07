# CSE230Project

This project will implement the popular Texas Hold’em poker game with multiple players, using the brick library in Haskell. The rules of the implemented game will strictly follow the classic ones: Each player has 2 private cards at first, and there are 5 public cards revealed in 3 betting rounds, where each player needs to take turns with one action among check, call, raise and fold. Based on certain ranking rules, each player seeks the best 5 cards from any combination of the 7 cards: the 5 public cards and their 2 private cards. The player who has the best hand and has not folded by the end of 3 betting rounds wins all of the money bets (if multiple players have the same winning hand, they share the bets equally). Our project will implement the following Texas Hold’em features as our goals: 

1. Algorithms for determining hand card ranking and winning condition 
2. Basic game mechanisms: moving dealers, blinds, round progression and splitting pot
3. Basic game logic for player actions: check, call, raise and fold 
4. Front-end visualization of poker cards, bets, and different game stages, using the brick library
5. Multi-players option on LAN (optional if time permits)
6. Simple AI to play against (optional if time permits)

Once the project is finished, the users should be able to use the program to start a fully simulated Texas Hold’em game, take turns to enter their actions, and finally win bets with their luck and strategies. 

# Build and Run
After cloning the repo and cd into the project directory, simply run 'cabal build' and 'cabal run client' to run the executable.

Press 'Esc' to exit the program. Press 'r' to reset the game. 

Each player will take turns to click one of the actions (Fold, Pass, Add 1, All In) on the right side to process the game. When one player has finished clicking the action, click next to switch to the next player (This will only proceeds to next player if the current action is valid)

<img width="321" alt="Screen Shot 2022-12-07 at 11 02 33" src="https://user-images.githubusercontent.com/30495478/206272410-09853f8e-094f-4d56-88c4-60a597e876d1.png">

After the last round was finished, the game will decide the one or more winners with best hand to take the income in the pot. And the hand of the the winner will be shown. Then the game will proceed to next one. 

<img width="318" alt="Screen Shot 2022-12-07 at 11 02 56" src="https://user-images.githubusercontent.com/30495478/206272462-dd3d35b1-d864-43ae-8f59-022517cebbef.png">

# Build and Run
