# CSE230Project

This project will implement the popular Texas Hold’em poker game with multiple players, using the brick library in Haskell. The rules of the implemented game will strictly follow the classic ones: Each player has 2 private cards at first, and there are 5 public cards revealed in 3 betting rounds, where each player needs to take turns with one action among check, call, raise and fold. Based on certain ranking rules, each player seeks the best 5 cards from any combination of the 7 cards: the 5 public cards and their 2 private cards. The player who has the best hand and has not folded by the end of 3 betting rounds wins all of the money bets (if multiple players have the same winning hand, they share the bets equally). Our project will implement the following Texas Hold’em features as our goals: 

1. Algorithms for determining hand card ranking and winning condition 
2. Basic game mechanisms: moving dealers, blinds, round progression and splitting pot
3. Basic game logic for player actions: check, call, raise and fold 
4. Front-end visualization of poker cards, bets, and different game stages, using the brick library
5. Multi-players option on LAN (optional if time permits)
6. Simple AI to play against (optional if time permits)

Once the project is finished, the users should be able to use the program to start a fully simulated Texas Hold’em game, take turns to enter their actions, and finally win bets with their luck and strategies. 
