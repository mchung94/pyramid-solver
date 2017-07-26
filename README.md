# pyramid-solver
Quickly find optimal-length solutions to Pyramid Solitaire.

# Overview
pyramid-solver searches for optimal-length solutions to Pyramid Solitaire according to Microsoft Solitaire Collection rules.  This program is intended to help people who get stuck while playing and want to find out the solution.  It either finds a solution to clear the board (the 28 cards in the pyramid) or says there's no solution.

There is a 64-bit command line program for Windows available for download.

If you're interested in Score and Card Challenges (maximizing score or clearing cards of a certain rank), I have another project at [solitaire-player](https://github.com/mchung94/solitaire-player) that can help.  That can also play the game for you as well.

## Rules
- Pyramid Solitaire uses a single 52-card deck.
- The game starts with:
  - 28 cards face up in a pyramid formation on the table with each row being 1/2/3/4/5/6/7 cards.
  - The remaining 24 cards in an area called the deck, in a stack with only the top card showing.
  - There's also a waste pile that starts out empty.
- Aces always count as 1, Jacks are 11, Queens are 12, and Kings are 13.
- The goal is to remove all 28 pyramid cards on the table.  There can still be cards in the deck and waste piles.
- With the cards that are not covered by other cards on the table below it, and the cards on the top of the deck and waste piles, the player can:
  - Remove a pair of cards with ranks that add up to 13.
  - Remove a king by itself.
  - Draw a card from the top of the deck to the top of the waste pile.
  - When the deck is empty, recycle the waste pile cards back into the deck, so that the deck cycles through the cards in the same order again.
- The player can cycle through the deck cards 3 times, recycling twice.

## Performance
On an Intel i7-4770k CPU (3.5GHz, 3.9GHz max) with LispWorks on Windows 10:

| Decks                | Mean (ms) | Median (ms) | Maximum (ms) | Total (min:sec) |
|:-------------------- | ---------:| -----------:| ------------:| ---------------:|
| 1500 random decks    |      1110 |         250 |        58578 |           27:44 |  
| 998 solvable decks   |       538 |         281 |         8453 |           08:56 |
| 502 unsolvable decks |      2245 |          78 |        58578 |           18:48 |

The slowest I found was the following deck which took 58 seconds to verify it's unsolvable:
```
            Th
          2h  4d
        3h  Qd  8h
      9h  5d  Jc  Td
    7c  4c  Ts  Ac  9c
  8d  5s  2s  7h  6s  7s
2c  9d  Qs  3d  5c  5h  Ad
8s Js 6c 9s 4h Kh Jd 4s 2d 6d Ks Qc 3s 3c Kc 7d Tc Ah 6h Qh Kd 8c As Jh
```
8s is at the top of the deck and Jh is at the bottom.

# Usage

## Command Line program
1. Create a plain text file (in Notepad for example) containing the cards for the Pyramid Solitaire deck.  See the Pyramid above as an example.  You don't have to add spaces or newlines to make it look nice - the program will ignore any letter that isn't a card rank (A23456789TJQK) or suit (cdhs).
2. Open a command prompt.
3. Run "pyramid-solver.exe filename" where filename is the name of the text file containing the cards.
4. pyramid-solver.exe will detect problems such as missing cards, too many cards, or malformed cards.  After running it will either say no solution exists, or say how many steps are in the solution, followed by the steps required to solve the deck.

## Source Code
- To load the system:
  - (asdf:load-system :pyramid-solver)
- To run the tests:
  - (asdf:test-system :pyramid-solver)
- To find a solution to Pyramid Solitaire using a string representation of a card deck:
  - (ps:solve (ps:string->deck "6d 5h Ah Jd 4s Ks 6s 8c 2h 4d 9s Kd 6c Ad 8s Ac 5c 9d 7h 3h 8d 5s 4c Qc Jh Kc Kh 3c 3s 9c As 5d Qh Ts 4h 7s Td 9h Th 7c 8h 2c 7d Tc 2d 6h 2s Js Qd 3d Qs Jc"))
- If you want to, you can remove the (in-package #:pyramid-solver) from [src/pyramid-solver.lisp](src/pyramid-solver.lisp) and load that file on its own.

The card deck example above would look like this in the game:
```
            6d
          5h  Ah
        Jd  4s  Ks
      6s  8c  2h  4d
    9s  Kd  6c  Ad  8s
  Ac  5c  9d  7h  3h  8d
5s  4c  Qc  Jh  Kc  Kh  3c
top of deck-> 3s 9c As 5d Qh Ts 4h 7s Td 9h Th 7c 8h 2c 7d Tc 2d 6h 2s Js Qd 3d Qs Jc <- bottom of deck
```
  
Cards are two letters containing rank (A23456789TJQK) and suit (cdhs).  The solution returned is either NIL when no solution exists, or a list of the following types of steps to win the game:
- "Draw", which means draw a card from the top of the deck to the top of the waste pile
- "Recycle", which means draw the cards in the waste pile back onto the deck so that they will be drawn from the deck in the same order again
- A list of one or two cards to remove, for example ("Kd") to remove the King of Diamonds or ("6h" "7c") to remove the 6 of Hearts and 7 of Clubs together.

## Requirements
- ASDF for the system definition
- FiveAM for running the tests
- A Common Lisp implementation where 60-bit values (unsigned-byte 60) are fixnums
- Most of the time, it needs a few hundred MB of RAM, but some decks use a few GB of RAM

pyramid-solver is developed using LispWorks 7.0 64-bit on Windows but also tested with SBCL 1.3.18 64-bit.  With SBCL, I had to start it with the --dynamic-space-size set to something large (a few GB) to avoid heap exhaustion on some decks.


# Programming Notes
The rest of this document contains information for other programmers about how this program works.

## Design Priorities
1. **Limited Scope** - pyramid-solver only tries to clear the 28 pyramid cards in the fewest number of steps.  It does not try to maximize score or prioritize removing cards of a certain rank (Score and Card Challenges in Microsoft Solitaire Collection).  I have created another project to handle those cases.
2. **Correctness** - the solver must always find the shortest solution where one exists or else correctly report when it's impossible to remove all 28 pyramid cards.
3. **Works on LispWorks and SBCL** - the solver doesn't need to be portable to every environment, but it must work on both LispWorks and SBCL 64-bit.
4. **Speed** - try to find the solution as quickly as possible while still being correct and working on both LispWorks and SBCL.
5. **Memory Usage** - minimize memory usage unless there's a significant speed boost to be gained by using more memory.  It's common for reduced memory usage to increase speed as well.

## Design Dictionary
- **Rank**: card ranks are single characters, one of A 2 3 4 5 6 7 8 9 T J Q K.  Ranks are always uppercase.
- **Suit**: card suits are single characters, one of c d h s.  Suits are always lowercase.
- **Card**: a two-letter string consisting of a rank followed by a suit.
- **Card Value**: the numeric value of a card from its rank.  Aces are always 1, Jacks are 11, Queens are 12, and Kings are 13.  This is needed because you can remove pairs of cards that add up to 13, or remove Kings by themselves.
- **Deck**: a deck is a list of cards containing one of each card in a standard 52-card deck.
- **Pyramid**: the pyramid refers to the 28 cards on the table laid out in a pyramid formation.  Microsoft Solitaire Collection calls this the table.  Some other sources call this the tableau.
- **Stock**: the stock initially contains the remaining 24 cards of the deck.  The cards are face up but only the card on top is visible. Microsoft Solitaire Collection calls this the deck.
- **Waste Pile**: the waste pile is initially empty, but cards can be taken from the stock and moved face up onto the top of the waste pile.  When the stock is empty, the waste pile cards can be recycled (moved back into the stock pile) up to twice per game.
- **Flags**: flags are integers representing bit fields.  The nth bit of the integer represents whether or not the nth card in the deck exists or not.
- **Indexes**: integer values referring to cards in the deck.  Index N refers to the Nth card in the deck (counting starts from 0).
- **Masks**: masks are integers representing bit fields.  Masks are used to either single out cards you are looking for in the deck to check if they exist or not, or to remove cards from the deck.

## The Algorithm
This program uses the [A* algorithm](https://en.wikipedia.org/wiki/A*_search_algorithm) with an unwinnable state detection procedure.

### Heuristic Function
For each step while playing the game, the heuristic function calculates an estimate of how many more steps are needed to win the game.  The following calculation is [admissible](https://en.wikipedia.org/wiki/Admissible_heuristic) and [consistent](https://en.wikipedia.org/wiki/Consistent_heuristic):
1. Count how many cards of each rank are on the table.
2. Find the higher count of every matching rank pair.  For example, if there are two Sixes and three Sevens on the table, the higher count is three, so it would take a minimum of three steps to remove all the Sixes and all the Sevens.  Find the higher count for A/Q, 2/J, 3/T, 4/9, 5/8, and 6/7.
3. Calculate the sum of the number of kings plus each of the six counts in step 2.  This is the estimated number of steps to win the game.

### Unwinnable State Detection
The actual code is faster with some precalculation, but the overall process to find out if a state is unwinnable is:
```
for each card on the table that isn't a King:
    find all the cards with the rank that adds up to 13 as a potential match
    filter out the ones that are covering or covered by the card
    if there aren't any left then there's no way to remove the card, so it's unwinnable
```

## Key Ideas
1. The solver needs to avoid revisiting states by keeping track of visited states, otherwise it may take a really long time regardless of anything else we do.  For example Depth-First Search sometimes finds a non-optimal solution in just a few milliseconds, but there are always decks where it could run for hours unless it avoids revisiting states.
2. My initial straightforwardly-designed Breadth-First Search on 1500 random card decks took 55 hours to finish.  After changing to A* search, it took 41 hours.  From this, I learned that representing the state of the game using sequences of cards (such as vectors/lists/strings), no matter what card representation I used, was too slow.  38% of the run time was spent just checking if a state was already seen before.  Now the A* implementation runs through the same 1500 decks in under 28 minutes.
3. There are only 1430 valid arrangements of cards in the pyramid.  The rule is that a card can't be removed if there are still cards covering it from below.  This is small enough that it is feasible to precalculate things for every possible pyramid card arrangement given a deck of cards.
4. Cards don't really need to move in Pyramid Solitaire, unlike other games like FreeCell.  Cards in the pyramid never move, they just get removed.  And if the 24 stock/waste cards were in one array, just an index into the array pointing to the top card of the stock pile is sufficient to simulate all the card moves.  If all the cards with higher index are the rest of the stock pile, and all the cards with lower index are the waste pile, then:
   - Drawing a card from the stock to the waste pile can be done by incrementing the index.
   - Recyling the waste pile can be done by resetting the index to the first card in the array.


## State Representation
A state shows where all the cards are and which cycle through the stock cards we are on at each step of playing the game.

We just want to store the minimum amount of information in each state - only the things that can change from state to state.  We don't need to store sequences of cards in each state.  Instead, focus on storing flags and indexes, and create a mapping from index to card somewhere else when we need to look it up.

States are represented using a 60-bit integer, which is a fixnum in 64 bit LispWorks and SBCL:
- Bits 0-51: bit flags to indicate which cards in the 52 card deck have been removed so far (called DECK-FLAGS in the code)
  - Bits 0-27 are the 28 pyramid cards (called PYRAMID-FLAGS in the code)
  - Bits 28-51 are the 24 stock/waste cards
- Bits 52-57: a 6-bit integer from 28 to 52 referring to the index of the card at the top of the waste pile (called STOCK-INDEX in the code)
  - the cards with index higher than this are the rest of the stock
  - the cards with index lower than this are the waste pile, the closest remaining card below this index is the top of the waste pile (called WASTE-INDEX in the code)
  - if the stock index is 52, the stock is empty, and if the waste index is 27, the waste pile is empty
- Bits 58-59: a 2-bit integer from 1 to 3 indicating which cycle through the stock cards we are currently in (called CYCLE in the code)

The 52 bit flags map to this arrangement:
```
            00
          01  02
        03  04  05
      06  07  08  09
    10  11  12  13  14
  15  16  17  18  19  20
21  22  23  24  25  26  27

stock/waste cards: 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51
```

## Precalculations
Because there are only 1430 possible values for bits 0-27 of a state (the pyramid cards), we can take a deck of cards and quickly precalculate all possible values for some information we need for each state.

### Precalculations that are not specific to a deck of cards
These are precalculated ahead of time and known before the program runs.
- ```*PYRAMID-COVER-MASKS*``` - Hardcoded masks for each pyramid card, indicating which pyramid card indexes cover it from below.  The tests show how to calculate this.  These masks are used to check if a pyramid card is uncovered.
- ```*UNRELATED-CARD-MASKS*``` - Hardcoded masks for each pyramid card, with 0 set on the pyramid card indexes which are covering or covered by the card.  The tests show how to calculate this.  These masks are used to exclude cards which can't be removed with the pyramid card because they are blocking each other.
- ```*ALL-PYRAMID-FLAGS*``` - The 1430 possible values for PYRAMID-FLAGS.  This runs code to set the value at load time so I don't have to hardcode a giant list into the source code.
- ```*ALL-UNCOVERED-INDEXES*``` - For each value in ```*ALL-PYRAMID-FLAGS*```, a list of all the uncovered pyramid cards' indexes.
- ```*ALL-PYRAMID-INDEXES*``` - For each value in ```*ALL-PYRAMID-FLAGS*```, a list of all the remaining pyramid cards' indexes.

### Precalculations that happen for each deck of cards
- ```CARD-VALUES``` - Create a vector of each card's numeric value.  This can be used to check if a card index refers to a king, or if two card indexes add up to 13 and can be removed together.
- ```CARD-BUCKET-MASKS``` - create a vector for each card value (1 - 13), holding a mask with bits set for each card in the deck with that value.  So index 1 would be a mask showing the position of each Ace in the deck.

The four functions we need from a state can be assisted with these precalculations:
1. Check if the state is a goal state
   - We could precalculate this: only the PYRAMID-FLAGS value of 0 is a goal state.
   - But the solver doesn't precalculate this because it's easy to just check at runtime if bits 0-27 of a state are all zero.
2. Check if a state is unwinnable
   - For each remaining pyramid card that isn't a king, collect masks that check if there exists a card that can remove the pyramid card, using ```CARD-BUCKET-MASKS``` and ```*UNRELATED-CARD-MASKS*```.
   - At runtime, to check if a state is unwinnable, mask off the bits from the state using each mask, and if the result is zero, that means there is a card can't be removed from the pyramid.
3. Calculate the heuristic function on the state
   - There's only 1430 pyramid flag values, so there's only 1430 possible values for the heuristic function, which can be calculated using ```*ALL-PYRAMID-INDEXES*``` and ```CARD-VALUES```.
   - At runtime, we can just look up the value for the given state.
4. Calculate successor states for each possible action from the state
   - For each of the 1430 pyramid flag values of ```*ALL-UNCOVERED-INDEXES*```, generate a 2D array indexed by ```STOCK-INDEX``` and ```WASTE-INDEX```.  For each combination of all 3 values, look for all combinations of cards that can be removed and generate a list of card removal masks.
   - At runtime, when we generate successor states for each state, we have to handle drawing a card and recycling the waste pile, but every other action we can take is done by removing the cards indicated by each mask and then updating the stock index if necessary.

## Search Node Representation
In general, nodes for search algorithms like Breadth-First Search or A* have the following fields:
- State: the state represented by the search node
- Parent Node: the node with the previous state
- Action: the action taken from the previous state to reach this state
- Depth: the number of nodes from the initial state's node to this node

For memory usage improvements, we don't use a class or struct to represent search nodes.  Creating tens of millions of these uses a lot of memory.  Instead, nodes are just lists of states starting with the current state and going back to the initial state (so the same parent node is shared by each successor state).  But also as a speed optimization, the first element of each node is the depth of the node.  So the fields can be generated like this:
- State: ```(second node)```
- Parent State: ```(third node)```
- Parent Node (minus its depth optimization): ```(cddr node)```
- Action: Derive this by diffing the state and parent state with a logical bitwise XOR, and see what changed.
  - If the cycle changed (bits 58-59), then the waste pile was recycled.
  - If any bits from 0-51 changed, the cards at those indexes were removed.
  - Otherwise, the stock index changed without removing any cards, so the action was drawing a card from the stock to the waste pile.
- Depth: ```(first node)```

## Priority Queue Implementation
A general purpose [priority queue](https://en.wikipedia.org/wiki/Priority_queue) is unnecessary.  A [bucket queue](https://en.wikipedia.org/wiki/Bucket_queue) is very fast and makes sense for solving Pyramid Solitaire, since we only need to insert nodes, remove the minimum priority node, and check if the queue is empty.

### Minimum and Maximum Solution Lengths
The shortest possible solution to Pyramid Solitaire is 15 steps.  This happens if each step removes two table cards, except the last two which can't be removed together since one covers the other.  13 pairs of table cards + 2 more for the last two cards.

The longest possible solution to Pyramid Solitaire would be 102 steps:
1. Draw 24 times (24 steps so far)
2. Recycle the waste pile (25 steps so far)
3. Draw 24 more times (49 steps so far)
4. Recycle the waste pile again (50 steps so far)
5. Draw 24 more times (74 steps so far)
6. Remove 24 pyramid cards by pairing with a card on the waste pile, which has no Kings.  The stock and waste piles are now empty. (98 steps so far)
7. Remove the last 4 pyramid cards, which are Kings (102 steps).

I don't think there exists a deck where that would be the shortest possible solution, but in any case, a bucket queue with buckets 0 to 102 would work for our priority queue.
