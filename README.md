# pyramid-solver
Quickly find optimal-length solutions to Pyramid Solitaire.

# Overview
pyramid-solver searches for optimal-length solutions to Pyramid Solitaire according to Microsoft Solitaire Collection rules.  This program is intended to help people who get stuck while playing and want to find out the solution.  There is now a 64-bit command-line program for Windows available for download.

## Rules
- Pyramid Solitaire uses a single 52-card deck.
- The game starts with 28 cards face up in a pyramid formation on the table with each row being 1/2/3/4/5/6/7 cards, and the remaining 24 cards on the deck in a stack with only the top card showing.  There's also a waste pile that starts out empty.  Sometimes the table is called the tableau and the deck the stock.
- Aces always count as 1, Jacks are 11, Queens are 12, and Kings are 13.
- The goal is to remove all 28 cards on the table.  There can still be cards in the deck and waste piles.
- With the cards that are not covered by other cards on the table below it, and the cards on the top of the deck and waste piles, the player can:
  - Remove a pair of cards with ranks that add up to 13
  - Remove a king by itself
  - Draw a card from the top of the deck to the top of the waste pile
  - When the deck is empty, recycle the waste pile cards back into the deck
- The player can cycle through the deck cards 3 times, recycling twice.

## Performance
On an Intel i7-4770k CPU (3.5GHz, 3.9GHZ max) with LispWorks on Windows 10:

| Decks                | Average (milliseconds) | Median (milliseconds)| Maximum (milliseconds) | Total (min:sec) |
|:-------------------- | ----------------------:| --------------------:| ----------------------:| ---------------:|
| 1500 random decks    |                   1585 |                  391 |                  83157 |           39:37 |  
| 998 solvable decks   |                    780 |                  438 |                  11609 |           12:59 |
| 502 unsolvable decks |                   3183 |                   62 |                  83157 |           26:38 |

The slowest I found was the following deck which took 83 seconds to verify it's unsolvable:
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
  - (ps:solve "6d 5h Ah Jd 4s Ks 6s 8c 2h 4d 9s Kd 6c Ad 8s Ac 5c 9d 7h 3h 8d 5s 4c Qc Jh Kc Kh 3c 3s 9c As 5d Qh Ts 4h 7s Td 9h Th 7c 8h 2c 7d Tc 2d 6h 2s Js Qd 3d Qs Jc")
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
- (Recommended) A Common Lisp implementation where 60-bit values (unsigned-byte 60) are fixnums.  It could probably still work otherwise but I haven't tested it.
- Most of the time, it needs a few hundred MB of RAM, but some decks use a few GB of RAM

pyramid-solver is developed using LispWorks 7.0 64-bit on Windows but also tested with SBCL 1.3.14 64-bit on Linux.  With SBCL, I had to start it with the --dynamic-space-size set to something large (a few GB) to avoid heap exhaustion on some decks.


# Programming Notes
The rest of this document contains information for other programmers about how this program works.

## The Algorithm
This program uses the [A* algorithm](https://en.wikipedia.org/wiki/A*_search_algorithm) with a basic unwinnable state detection procedure.  A lot of work went into lower-level optimizations, which I will explain below.

### Heuristic Function
For each step while playing the game, the heuristic function calculates an estimate of how many more steps are needed to win the game.  The following calculation is [admissible](https://en.wikipedia.org/wiki/Admissible_heuristic) and [consistent](https://en.wikipedia.org/wiki/Consistent_heuristic):
1. Count how many cards of each rank are on the table.
2. Find the higher count of every matching rank pair.  For example, if there are two Sixes and three Sevens on the table, the higher count is three, so it would take a minimum of three steps to remove all the Sixes and all the Sevens.  Find the higher count for A/Q, 2/J, 3/T, 4/9, 5/8, and 6/7.
3. Calculate the sum of the number of kings plus each of the six counts in step 2.  This is the estimated number of steps to win the game.

### Repeated State Avoidance
pyramid-solver keeps a hash table of successor states it has seen during expansion and skips the same state if it sees it again, unless it has reached the same state with a shorter number of steps.  Basically it's trying to filter out some non-optimal nodes before they are even added to the fringe.

### Unwinnable State Detection
The actual code is faster with some precalculation, but the overall process to find out if a state is unwinnable is:
```
for each card on the table that isn't a King:
    find all the cards with the rank that adds up to 13 as a potential match
    filter out the ones that are covering or covered by the card
    if there aren't any left then there's no way to remove the card, so it's unwinnable
```

## Other Ideas Explored
1. Breadth First Search with repeated state detection can be faster at finding out if a deck is unsolvable but is generally slower at solvable decks.
2. Depth First Search with prioritized moves: I was thinking along the lines of assigning ranks to potential moves, like removing two table cards is better than removing one table card plus a card on the deck, or removing a King.  Sometimes this helps Depth First Search find non-optimal solutions really fast.  But this still had worst-case performance that was very slow.
3. Instead of detecting and avoiding repeated states, detect and avoid "similar" states and look for non-optimal solutions.  For example instead of checking if the table/deck/waste/cycle were all exactly the same, what if pyramid-solver only checked if the table cards and the top cards of the deck and waste pile were all the same, even though the cards under the top were different?  I had some impressive results for some decks, but it can fail to find a solution even though one exists.
4. Iterative Deepening Depth First Search - this conserves memory compared to Breadth First Search but I was more concerned with speed.  However, I did use Depth-Limited search in a solution verification program to check pyramid-solver, to verify that no shorter solution exists.
5. I spent way too much time evaluating possible representations for cards (as symbols, strings, class instances, structs, integers, single characters, conses holding rank and suit), but in the end, it didn't matter because of precalculation.
6. I was inspired by the essay [Solving Every Sudoku Puzzle](http://norvig.com/sudoku.html) to try constraint propagation for unwinnable state detection.  I tried using a basic strategy of "if there is only one card available to remove a table card, remove that card as a possibility for its peers of the same rank".  But the additional processing slowed it down overall, so I stopped investigating it because I currently don't have a lot of good ideas on additional constraint strategies to propagate.

## Fixing Performance Issues
Initially the A* search was quite slow.  A simple Breadth-First search took 55 hours to run through the 1500 random card decks.  My first A* implementation took 41 hours to do the same, but my current version does it in 39 minutes.  I took the following steps to reach acceptable performance without changing the high-level algorithms.

### Performance Issue 1: Repeated State Avoidance
To avoid processing repeated states, pyramid-solver needs to keep track of states it has visited.  The process of finding out if it has already seen a state, using a hash table, was about 38% of the total run time according to the profiler.

#### Initial State Representation
Cards are represented as two-letter strings.  Each state was an object that consisted of:
- The Table - a 28-element array of cards
- The Deck - a list of cards
- The Waste Pile - a list of cards
- Cycle - A number from 1 - 3 to indicate how many times you've cycled through the deck cards.

#### Insights
The first insight I had was the idea that in Pyramid Solitaire, cards aren't really moving around like they do in Klondike or FreeCell.  The cards just get removed, and you can simulate drawing cards from the deck to the waste pile by keeping the cards in an array and just updating an index to the top of the deck.

For speed, I was thinking of implementing cards as integers from 0 to 51 where Ac=0, 2c=1, 3c=2, .... Qs=50, Ks=51.  This way, cards can be treated as an index or ID that you can use to look up anything you want about the card.  For example the rank could be a lookup into a string like this:
```common-lisp
(defun card-rank (card-id)
  (schar "A23456789TJQKA23456789TJQKA23456789TJQKA23456789TJQK" card-id))
```
But the second insight I had was that if you're playing Pyramid Solitaire with a deck, for example the one shown in the Performance section, why can't the lookup be precalculated into something like:
```common-lisp
(defun card-rank (deck-index)
  (schar "T243Q895JT74TA985276729Q355A8J694KJ426KQ33K7TA6QK8AJ" deck-index))
```
So the first card in the deck is a Ten, the second is a 2, etc.

With these two ideas, I now had a bunch of ideas on things I can precalculate:
- What is the Nth card in the deck?
- Is the Nth card a king?
- What is the numeric rank of the Nth card?
- Do the Nth card and the Mth card have ranks that add up to 13?
- What are the indexes of the cards can be removed with the Nth card on the table?

A lot of things can be converted into a precalculated array lookup which is a lot faster than card analyzing logic.

Even better, once I precalculated everything, I found I could store a lot less information in the state itself, to the point where it could just be a single 60-bit integer.

#### New State Representation - 60 bits
- Bits 0-51: The Nth bit indicates if the Nth card in the deck remains or has been removed.
  - Bits 0-27 are the 28 table cards
  - Bits 28-51 are the 24 deck/waste pile cards
- Bits 52-57: A 6-bit integer from 28 to 52 indicating the deck-index
  - The card at the deck-index is the top of the deck, waste-index can be derived to point to the top of the waste pile
  - The cards above the deck-index are the rest of the deck
  - If the deck-index is 52 the deck is empty
  - The cards below the deck-index are the waste pile
  - The waste card closest to the deck-index is the top of the waste pile
  - If the waste-index is 27 the waste pile is empty.
- Bits 58-59: A 2-bit integer from 1 to 3 indicating which cycle through the deck we are currently in

Now, comparing states for equality is just comparing two fixnums for equality using EQL.  This is much faster and also lowers memory usage compared to the original state representation.

#### Unwinnable States and Uncovered Table Cards
With these card existence flag bits, there's a really fast way to do unwinnable state detection and finding uncovered table cards.  I can precalculate bit masks and perform bitwise logic to say for example, if the card at index 12 exists but the ones at indexes 34 and 45 don't, then it's unwinnable - that's now done with a single array index lookup, a logical bitwise AND, a left shift and a fixnum comparison.
```common-lisp
;;; index 0 is on the right and 51 is on the left
;;; precalculated 52-bit mask for cards at indexes 12, 34, and 45
;;; for this deck, card 34 and 45 are the only ones you can use to remove card 12
;;; 0000001000000000010000000000000000000001000000000000
;;; 52-bit card existence flags
;;; 1111110101111110000000000000000000000001111111111111
(eql (ash 1 12) (logand mask exist-flags))
;;; this is equivalent to asking if card 12 has not been removed yet, but 34 and 45 have been removed
```
Or if the table card at index 12 exists but the ones at indexes 17 and 18 don't, then the table card at index 12 is uncovered - that would be the exact same series of operations but with a different bit mask.

### Performance Issue 2: Priority Queue
After fixing the first performance issue, the profiler showed that almost 60% of the run time was spent inserting and removing search nodes from a general purpose [priority queue](https://en.wikipedia.org/wiki/Priority_queue).  Wikipedia mentioned the [Bucket Queue](https://en.wikipedia.org/wiki/Bucket_queue) which makes sense for solving Pyramid Solitaire, since we only need to insert nodes, remove the minimum priority node, and check if the queue is empty.

#### Minimum and Maximum Solution Lengths
The shortest possible solution to Pyramid Solitaire is 15 steps.  This happens if each step removes two table cards, except the last two which can't be removed together since one covers the other.  13 pairs of table cards + 2 more for the last two cards.

The longest possible solution to Pyramid Solitaire would be 100 steps:
1. Draw 24 times
2. Recycle the waste pile
3. Draw 24 more times
4. Recycle the waste pile again
5. Draw 24 more times
6. Remove 24 table cards by pairing with a card on the waste pile, then remove 4 more at some point as pairs of table cards because there are only 24 cards in the waste pile.

I don't think there exists a deck where that would be the shortest possible solution, but in any case, a bucket queue with buckets 0 to 100 would work for our priority queue.  This helped speed up the program a lot.

### Performance Issue 3: Creating Nodes
Search Nodes were implemented like this:
```common-lisp
(defclass node ()
  ((state :reader node-state
          :initarg :state
          :initform *initial-state*
          :type state
          :documentation "The Pyramid Solitaire state for this search node.")
   (parent :reader node-parent
           :initarg :parent
           :initform nil
           :type node
           :documentation "The parent node predecessor to the state.")
   (action :reader node-action
           :initarg :action
           :initform 0
           :type fixnum
           :documentation "A representation of the action taken from the
                           parent node's state to reach this node's state.")
   (depth :reader node-depth
          :initarg :depth
          :initform 0
          :type (integer 0 100)
          :documentation "The search node's depth.")
  (:documentation "A search node for Pyramid Solitaire."))
  
(defun make-node (&key state parent action depth)
  (make-instance 'node :state state :parent parent :action action :depth depth))
```
I was surprised by this but after fixing the first two performance issues, the profiler showed that 24% of the run time was spent in make-node.  With the long-running unsolvable deck described in the Performance section, 53 million nodes were created in 83 seconds total run time, so 24% of 83 seconds is about 20 seconds of creating nodes, or around 2,650,000 could be created per second.  I didn't investigate much because it's easy to switch to defstruct and see how that affects performance.

Changing node to a defstruct helped a lot in terms of speed, and I changed it to an unnamed struct to try to lower memory usage:
```common-lisp
(defstruct (node (:type vector))
  "A search node used for A* search."
  (state *initial-state* :type state)
  (parent nil :type (or null (simple-vector 4)))
  (action 0 :type action)
  (depth 0 :type (integer 0 100)))
```

**But I got significant memory savings by getting rid of node structures entirely.**  Now nodes are just a list of states starting with the current state and going back to the initial state, but with the first element being the depth of the node (as an optimization so pyramid-solver doesn't spend time calculating the length of the list).  To create a new successor state node given a parent node, you just cons your new state onto the cdr of the parent node (skipping over where the parent node holds its depth), then cons the new depth on top of that.  The parent node is unaffected by this.

On any given node:
- node's depth = (first node)
- node's state = (second node)
- node's parent = (third node)
- the action to get from parent to state = diff the state and parent with logical XOR, then derive the action that took place.
- every state from the initial state to the current state = (reverse (rest node)), and the list of actions to go from the initial state to the current state can be derived from this.
