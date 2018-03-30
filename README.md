# pyramid-solver
Quickly find optimal solutions to Pyramid Solitaire.

# Overview
pyramid-solver searches for optimal length solutions to Pyramid Solitaire according to Microsoft Solitaire Collection rules.  This program is intended to help people who get stuck while playing and want to find out the solution to Board Challenges.  It either finds a solution to clear the board (the 28 cards in the pyramid) or says there's no solution.

There is a 64-bit command line program for Windows available for download.

If you're interested in Score and Card Challenges (maximizing score or clearing cards of a certain rank), I have another project at [solitaire-player](https://github.com/mchung94/solitaire-player) that can help.  That can also play the game for you as well if you're playing Microsoft Solitaire Collection for Windows 10.

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

| Decks                | Mean (ms) | Standard Deviation (ms) | Median (ms) | Maximum (ms) | Total (min:sec) |
|:-------------------- | ---------:| -----------------------:| -----------:| ------------:| ---------------:|
| 1500 random decks    |       940 |                    2975 |         281 |        46813 |           23:29 |  
| 998 solvable decks   |       492 |                     584 |         312 |         6750 |           08:10 |
| 502 unsolvable decks |      1830 |                    4958 |         140 |        46813 |           15:19 |

The slowest I found was the following deck which took 46 seconds to verify it's unsolvable:
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
1. Create a plain text file (in Notepad for example) containing the cards for the Pyramid Solitaire deck.  See the Pyramid above as an example.  You don't have to add spaces or newlines to make it look nice - the program searches for any card rank (A23456789TJQK) followed by suit (cdhs).
2. Open a command prompt.
3. Run "pyramid-solver.exe filename" where filename is the name of the text file containing the cards.
4. pyramid-solver.exe will detect problems such as missing cards, too many cards, or malformed cards.  After running it will either say no solution exists, or say how many steps are in the solution, followed by the steps required to solve the deck.

## Source Code
- To load the system:
  - (asdf:load-system "pyramid-solver")
- To run the tests:
  - (asdf:test-system "pyramid-solver")
- To find a solution to Pyramid Solitaire using a string representation of a card deck:
  - (ps:solve (ps:string->card-list "6d 5h Ah Jd 4s Ks 6s 8c 2h 4d 9s Kd 6c Ad 8s Ac 5c 9d 7h 3h 8d 5s 4c Qc Jh Kc Kh 3c 3s 9c As 5d Qh Ts 4h 7s Td 9h Th 7c 8h 2c 7d Tc 2d 6h 2s Js Qd 3d Qs Jc"))
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
  
Cards are two letters containing rank (A23456789TJQK) followed by suit (cdhs).  The solution returned is either NIL when no solution exists, or a list of the following types of steps to win the game:
- "Draw", which means draw a card from the top of the deck to the top of the waste pile
- "Recycle", which means draw the cards in the waste pile back onto the deck so that they will be drawn from the deck in the same order again
- A list of one or two cards to remove, for example ("Kd") to remove the King of Diamonds or ("6h" "7c") to remove the 6 of Hearts and 7 of Clubs together.

## Requirements
- ASDF for the system definition
- FiveAM for running the tests
- A Common Lisp implementation where 52-bit values (unsigned-byte 52) are fixnums
- Most of the time, it needs a few hundred MB of RAM, but some decks use a few GB of RAM

pyramid-solver is developed using LispWorks 7.1 64-bit on Windows but also tested with SBCL 1.4.2 64-bit.  With SBCL, I had to start it with the --dynamic-space-size set to something large (a few GB) to avoid heap exhaustion on some decks.

# Programming Notes
The rest of this document contains information for other programmers about how this program works.

## Design Priorities
1. **Limited Scope** - pyramid-solver only tries to clear the 28 pyramid cards in the fewest number of steps.  It does not try to maximize score or prioritize removing cards of a certain rank (Score and Card Challenges in Microsoft Solitaire Collection).  I have created another project to handle those cases.
2. **Correctness** - the solver must always find the shortest solution when one exists or else correctly report when it's impossible to remove all 28 pyramid cards.
3. **Works on LispWorks and SBCL** - the solver doesn't need to be portable to every environment, but it must work on both LispWorks and SBCL 64-bit.
4. **Speed** - try to find the solution as quickly as possible while still being correct and working on both LispWorks and SBCL.
5. **Memory Usage** - minimize memory usage unless there's a significant speed boost to be gained by using more memory.

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
- **Masks**: masks are integers representing bit fields.  Masks are used to either single out cards you are looking for in the deck to check if they exist or not, remove cards from the deck, or transform a state into a successor state.

## The Search Algorithm
The solver uses the [A\* algorithm](https://en.wikipedia.org/wiki/A*_search_algorithm) to find the fewest number of steps needed to remove all 28 pyramid cards, or find out if it's impossible to remove them all.  This is on average much faster than Breadth-First Search when a solution exists, and only a little slower when there is no solution.

To narrow down the search possibilities, the solver keeps track of visited states and avoids revisiting a state unless it found a quicker way to reach that state.  It also has a procedure to check each state to see if there's a card in the pyramid that can no longer be removed, and stops investigating those states.

## Card and Deck Representation
Cards are two-letter strings containing a rank character followed by a suit character.  This makes cards human-readable and easy to work with.  A list of cards is a deck, and a deck containing exactly all 52 cards is called a standard deck in the code.  It's not important to optimize the card and deck representation, because the solver precalculates some data using the deck before searching for a solution, and never refers to the deck again until after it finds a solution.

## The Pyramid, Stock Pile, and Waste Pile
The first 28 cards of the deck are laid out in a pyramid formation, indexed from 0 to 27:
```
            00
          01  02
        03  04  05
      06  07  08  09
    10  11  12  13  14
  15  16  17  18  19  20
21  22  23  24  25  26  27
```

A card can only be removed when the cards covering it from below are removed first.  For example, card 12 can't be removed from the pyramid until cards 17, 18, 23, 24, and 25 are removed.  Because of this rule, there are only 1430 valid ways for cards to be laid out in the pyramid while playing Pyramid Solitaire, and the solver takes advantage of this for precalculations and state representation.

The last 24 cards of the deck are used for the stock and waste piles, indexes 28-51 in the deck.  Initially, card 28 is the top of the stock pile, card 51 is the bottom, and the waste pile is empty.

## Precalculated Data for the Pyramid
Type Declarations:
- `PYRAMID-FLAGS`: 28-bit unsigned values used as bit flags where the n-th bit indicates if the n-th card in the pyramid remains or has been removed from the pyramid.
- `PYRAMID-INDEX`: an index, 0 to 27, referring to the first 28 cards of the deck and their position in the pyramid.
- `PYRAMID-ID`: an identifier, 0 to 1429, referring to each possible value of PYRAMID-FLAGS.

The following data is the same for any deck of cards:
- `*PYRAMID-FLAGS*`: A vector of all 1430 possible PYRAMID-FLAGS values in sorted order.  The index into this vector is the definition of PYRAMID-ID.
- `*PYRAMID-FLAGS->ID*`: A hashtable mapping PYRAMID-FLAGS values back into their PYRAMID-IDs.
- `*PYRAMID-EXISTING-INDEXES*`: A vector, indexed by PYRAMID-ID, where each value is a list of the PYRAMID-INDEXes of the cards remaining in the pyramid.
- `*PYRAMID-UNCOVERED-INDEXES*`: A vector, indexed by PYRAMID-ID, where each value is a list of the PYRAMID-INDEXes of the remaining uncovered cards in the pyramid.

## State Representation
For speed we want a state representation where checking if two states are equal is very fast.  The key idea is that we can build the state representation out of data that refers to the deck of cards, without having to store the actual deck of cards in the state itself.  Then we can pack all the data into a fixnum so that checking if two states are equal is the same as checking if two small integers are equal.

To represent the state of the game at each step of play, we need to know:
1. The cards remaining in the pyramid:
   - We use 11 bits to store a `PYRAMID-ID` value to describe this (an integer from 0 to 1429).
   - This is an index into all precalculated pyramid data described above.
2. How many times the player has recycled the waste pile:
   - We use 2 bits to store a `CYCLE`, an integer from 0 to 2
3. The cards in the stock and waste piles:
   - We use 24 bits to indicate which of the last 24 cards in the deck remain in the game.
   - We use 6 bits to store a `STOCK-INDEX`, an index to the top card of the stock pile (an integer from 28 to 52, where 52 means the stock pile is empty).
   - The remaining cards with higher index than `STOCK-INDEX` are the rest of the stock, and the remaining cards with lower index than `STOCK-INDEX` are the waste pile.
   - The `WASTE-INDEX` is derived from the `STOCK-INDEX` and 24 bit flags by finding the nearest existing card with index lower than `STOCK-INDEX`, or 27 to indicate the waste pile is empty.
   - To draw a card from the stock pile to the waste pile, increment the index until it refers to the next existing card.  If it goes past the end of the deck at index 51, it's empty.
   - To recycle the waste pile, reset the index to 28 and and increment it until it refers to the next existing card.

So the state is a 52-bit value containing the following pieces of information:
- Bits 0-10: a `PYRAMID-ID`
- Bits 11-12: a `CYCLE`
- Bits 13-18: a `STOCK-INDEX`
- Bits 19-27: unused padding, all zeros, so bits 28-51 can refer to the stock/waste cards which are cards 28-51 in the deck
- Bits 28-51: bit flags indicating which stock/waste cards remain (out of the last 24 cards of the deck)

One thing to note is that we can look up the `PYRAMID-FLAGS` (28 bits) from the `PYRAMID-ID`, and combine it with bits 28-51 of a state, to get a 52-bit value indicating which cards in the 52-card deck remain in the game.  These are called `DECK-FLAGS` in the code.

## Precalculated Data for each Deck of Cards

### Card Values
The `CARD-VALUES` function takes a deck of cards and returns a vector containing each card's numeric value.  The remaining precalculations use this vector instead of the list of cards for faster indexing and also checking which pairs of cards add up to 13.

### Successor Masks
Given a state, the successor states are the states that result when the player performs an action.

The actions the player can take are:
1. Draw a card from the stock pile to the waste pile if the stock isn't empty.
2. Recycle the waste pile if the stock pile is empty and the player hasn't already recycled twice.
3. Given the uncovered cards on the pyramid and the top cards of the stock pile and waste pile, remove a King or a pair of cards that add up to 13.

A successor mask is a value that be combined with a state using exclusive or (XOR) to return a successor state.

The way XOR works for states:
- Precalculate: `(LOGXOR starting-state successor-state)` => successor-mask
- While searching for a solution: `(LOGXOR starting-state successor-mask)` => successor-state

During the precalculation phase, we don't actually know the entire successor state value - there's too many possible values for the stock/waste bit flags.  But we do know the successor state's `PYRAMID-ID`, `CYCLE`, and whether or not the `STOCK-INDEX` needs to be incremented, and that's what the successor mask changes.

The `SUCCESSOR-MASKS` function precalculates every successor mask for a deck of cards by taking each possible combination of `PYRAMID-ID`, `STOCK-INDEX`, `WASTE-INDEX`, and `CYCLE`, and calculating a list of successor masks for it.

When the solver runs, it takes a state and extracts its `PYRAMID-ID`, `STOCK-INDEX`, `WASTE-INDEX`, and `CYCLE` from it.  Then the solver looks up the list of successor masks and XORs each with the starting state to get a successor state.  The resulting successor state isn't exactly correct yet - the function `STATE-ADJUST-STOCK-INDEX` fixes the successor state's stock index to point to an existing card or 52 if the stock is empty.

### A\* Heuristic Function
For each step while playing the game, the heuristic function calculates an estimate of how many more steps are needed to win the game.  The following calculation is [admissible](https://en.wikipedia.org/wiki/Admissible_heuristic) and [consistent](https://en.wikipedia.org/wiki/Consistent_heuristic):
1. Count how many cards of each rank are on the pyramid.
2. For every pair of matching ranks that add up to 13 (A/Q, 2/J, 3/T, 4/9, 5/8, and 6/7), find out which rank appears more often in the pyramid, and make note of how many cards there are of that rank.  For example, if there are two Sixes and three Sevens in the pyramid, there are more Sevens.  So it would take a minimum of three steps to remove all the Sixes and all the Sevens from the pyramid.
3. Calculate the sum of the number of kings plus each of the six counts in step 2.  This is the estimated number of steps to win the game.

### Unclearable Pyramid Detection
For any state, if there exists a pyramid card that can't be removed, then it's considered unclearable, and not worth investigating further when searching for the quickest way to clear the pyramid cards.

For example, in this pyramid below, there are four Jacks in the pyramid, but there's a 2d at the top of the pyramid.  The 2d can never be removed because all four Jacks need to be removed first to uncover it, and we can't remove all four Jacks without using the 2d to pair with one of the Jacks for removal.
```
            2d
          9s  7c
        5d  2s  Qc
      Jd  5c  Jc  Td
    4s  6s  8c  8s  Jh
  5h  As  Js  6d  2c  Qd
Qh  4c  8h  Ks  7d  Ah  4d
```

The function `UNCLEARABLE-MASKS` calculates lists of masks to be ANDed with the state and if any result is zero, then the state's pyramid is unclearable.  For each `PYRAMID-ID`, the function only considers cards that aren't kings and don't already have another pyramid card it can be removed with.  Then it creates a mask for each of these cards, singling out the matching cards in the last 24 cards in the deck (the stock/waste piles).

## Search Node Representation
In general, nodes for search algorithms like Breadth-First Search or A\* have the following fields:
- State: the state represented by the search node
- Parent Node: the node with the previous state
- Action: the action taken from the previous state to reach this state
- Depth: the number of nodes from the initial state's node to this node

For memory usage improvements, we don't use a class or struct to represent search nodes.  Creating tens of millions of these uses a lot of memory.  Instead, nodes are just lists of states starting with the current state and going back to the initial state (so the same parent node is shared by each successor state's node).  But also as a speed optimization, the first element of each node is the depth of the node.  So the fields can be generated like this:
- State: `(second node)`
- Parent State: `(third node)`
- Parent Node (minus its depth optimization): `(cddr node)`
- Action: Derive this by diffing the state and parent state with a logical bitwise XOR, and see what changed.
  - If the `CYCLE` changed (bits 11-12), then the waste pile was recycled.
  - If the `PYRAMID-ID` (bits 0-10) or the 24 stock/waste bit flags changed (bits 28-51), then cards were removed.
  - Otherwise, the action was drawing a card from the stock to the waste pile.
- Depth: `(first node)`

If we were to use Breadth-First Search we could reuse the hashtable of visited states by using the state as the key and its parent state as its value.

## Priority Queue Implementation
A general purpose [priority queue](https://en.wikipedia.org/wiki/Priority_queue) is unnecessary.  A [bucket queue](https://en.wikipedia.org/wiki/Bucket_queue) is very fast and makes sense for solving Pyramid Solitaire, since we only need to insert nodes, remove the minimum priority node, and check if the queue is empty.

### Minimum and Maximum Solution Lengths
To find out how big we need to make the bucket queue, we need to know the maximum number of steps a game of Pyramid Solitaire could take.

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

## Keeping Track of Visited States
The simplest way for the solver to keep track of visited states is to use a hashtable where the states are the keys.

But the code uses a vector of hashtables, indexed by the first 13 bits of the state (containing PYRAMID-ID and CYCLE).  This helps performance (in LispWorks at least) by partitioning the states so that as the hashtables grow and need to be rehashed, we don't have to rehash every state.
