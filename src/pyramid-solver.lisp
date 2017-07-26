(in-package #:pyramid-solver)

;;; Card and Deck related definitions
;;; These functions don't have to be fast because the first thing we do is
;;; use them to precalculate other stuff that will be faster to use.

(defvar *rank-characters* "A23456789TJQK" "All valid rank characters.")
(defvar *suit-characters* "cdhs" "All valid suit characters.")

(defun cardp (card)
  "Return a true value if CARD is a string containing rank followed by suit."
  (and (stringp card)
       (= 2 (length card))
       (find (schar card 0) *rank-characters*)
       (find (schar card 1) *suit-characters*)))

(deftype card ()
  "Cards are two-letter strings containing rank followed by suit."
  '(and (simple-base-string 2) (satisfies cardp)))

(defun make-card (rank suit)
  "Create a card from RANK and SUIT.  Don't signal an error here on bad input.
More descriptive error reporting is possible using the deck as a whole."
  (format nil "~A~A" rank suit))

(defun rank (card)
  "Return CARD's character rank."
  (schar card 0))

(defun value (card)
  "Return CARD's numeric value.  A = always 1, J = 11, Q = 12, K = 13."
  (1+ (position (rank card) *rank-characters*)))

(defun collect-card-chars (string)
  "Return a list of all card rank and suit characters in STRING.
Ranks will become uppercase and suits will become lowercase."
  (loop for ch across string
        when (or (find (char-downcase ch) *suit-characters*)
                 (find (char-upcase ch) *rank-characters*))
        collect it))

(defvar *all-cards*
  (loop for suit across *suit-characters*
        nconc (loop for rank across *rank-characters*
                    collect (make-card rank suit)))
  "All the cards in a 52-card deck.")

(defun deckp (deck)
  "Return T if DECK is a list containing all 52 cards in a standard deck.
The four additional values returned are: the number of cards, the missing
cards, the duplicated cards, and the malformed cards.  If DECK is not a list,
it will return NIL and all 52 cards as missing."
  (if (listp deck)
      (flet ((uniquep (card)
               (= 1 (count card deck :test #'string=))))
        (let ((num-cards (length deck))
              (missing (set-difference *all-cards* deck :test #'string=))
              (duplicates (remove-if #'uniquep deck))
              (malformed (remove-if #'cardp deck)))
          (values (not (or (/= 52 num-cards) missing duplicates malformed))
                  num-cards missing duplicates malformed)))
    (values nil 0 (copy-list *all-cards*) nil nil)))
    
(deftype deck ()
  "Decks are lists containing all the cards in a standard 52-card deck."
  '(and list (satisfies deckp)))

(defun string->deck (string)
  "Convert a string containing card characters into a deck of cards."
  (loop for (rank suit) on (collect-card-chars string) by #'cddr
        collect (make-card rank suit)))



;;; Deck bit flags and index related definitions
(deftype deck-flags ()
  "Bit flags for the existence of each card in a 52-card deck."
  '(unsigned-byte 52))

(deftype deck-index ()
  "An index into the 52-card deck."
  '(integer 0 51))

(deftype pyramid-flags ()
  "Bit flags for the first 28 cards in the deck arranged in a pyramid.
This is bits 0-27 of a DECK-FLAGS value.
The indexes into the pyramid look like this:
            00
          01  02
        03  04  05
      06  07  08  09
    10  11  12  13  14
  15  16  17  18  19  20
21  22  23  24  25  26  27"
  '(unsigned-byte 28))

(deftype pyramid-index ()
  "An index into the 28 pyramid cards."
  '(integer 0 27))

(deftype stock-index ()
  "A deck index into the 24 stock/waste cards after the 28 pyramid cards.
This is bits 28-51 of a DECK-FLAGS value.  The card at the stock index is the
top card in the stock.  Cards with higher index in the deck are in the stock
as well."
  '(integer 28 52))

(defconstant +empty-stock-index+ 52
  "If the stock index is 52 it means the stock pile is empty.")

(declaim (inline stock-empty-p))
(defun stock-empty-p (stock-index)
  "Return T if the stock pile is empty according to the STOCK-INDEX."
  (declare (type stock-index stock-index)) 
  (eql stock-index +empty-stock-index+))

(deftype waste-index ()
  "A deck index into the 24 stock/waste cards after the 28 pyramid cards.
The card at the waste index is the top card in the waste pile.  Cards with
lower index in the deck are in the waste pile as well." 
  '(integer 27 51))

(defconstant +empty-waste-index+ 27
  "If the waste index is 27 it means the waste pile is empty.")

(declaim (inline waste-empty-p))
(defun waste-empty-p (waste-index)
  "Return T if the waste pile is empty according to the WASTE-INDEX."
  (declare (type waste-index waste-index))
  (eql waste-index +empty-waste-index+))



;;; Functions and precalculated data that apply to any deck of cards
(declaim (inline mask))
(defun mask (deck-index)
  "Given a DECK-INDEX, return a DECK-FLAGS mask with that bit set to 1."
  (declare (deck-index deck-index))
  (the deck-flags (ash 1 deck-index)))

(declaim (inline removal-mask))
(defun removal-mask (deck-index)
  "Return a mask that removes the card at DECK-INDEX from a DECK-FLAGS value."
  (declare (deck-index deck-index))
  (the deck-flags (mask-field (byte 52 0) (lognot (mask deck-index)))))

(declaim (inline card-exists-p))
(defun card-exists-p (index flags)
  "Return T if the card at INDEX still exists according to FLAGS.
This is the same as LOGBITP but faster with optimizations on LispWorks."
  (declare (deck-index index)
           (deck-flags flags))
  (not (zerop (logand (mask index) flags))))

(defvar *pyramid-cover-masks*
  #(#b1111111111111111111111111110
    #b0111111011111011110111011000
    #b1111110111110111101110110000
    #b0011111001111001110011000000
    #b0111110011110011100110000000
    #b1111100111100111001100000000
    #b0001111000111000110000000000
    #b0011110001110001100000000000
    #b0111100011100011000000000000
    #b1111000111000110000000000000
    #b0000111000011000000000000000
    #b0001110000110000000000000000
    #b0011100001100000000000000000
    #b0111000011000000000000000000
    #b1110000110000000000000000000
    #b0000011000000000000000000000
    #b0000110000000000000000000000
    #b0001100000000000000000000000
    #b0011000000000000000000000000
    #b0110000000000000000000000000
    #b1100000000000000000000000000
    #b0000000000000000000000000000
    #b0000000000000000000000000000
    #b0000000000000000000000000000
    #b0000000000000000000000000000
    #b0000000000000000000000000000
    #b0000000000000000000000000000
    #b0000000000000000000000000000)
  "For each pyramid card index, a mask of all cards covering it from below.
This is for checking if a pyramid card is uncovered and therefore available
for removal.  The bottom row of the pyramid has a mask of just 0 because they
aren't covered by any other cards.")

(declaim (inline card-uncovered-p))
(defun card-uncovered-p (pyramid-index pyramid-flags)
  "Return T if the card at PYRAMID-INDEX has no cards covering it from below.
An uncovered card is available to be removed if there's a matching uncovered
card to remove it with, or if it's a King."
  (declare (pyramid-flags pyramid-flags)
           (pyramid-index pyramid-index))
  (zerop (logand (the pyramid-flags (svref *pyramid-cover-masks* pyramid-index))
                 pyramid-flags)))

(defun all-pyramid-flags ()
  "Gather all 1430 possible valid values for pyramid-flags.
The rule is: if a card is gone, its children must be gone as well.  Knowing
that there are only 1430 possible values lets us precalculate information we
will use to run the solver faster."
  (declare (optimize speed (safety 0) (debug 1)))
  (flet ((invalidp (pyramid-flags)
           "It's impossible to have a card removed but descendants remaining."
           (declare (pyramid-flags pyramid-flags))
           (dotimes (i 21)
             (declare (type pyramid-index i))
             (unless (or (card-exists-p i pyramid-flags)
                         (card-uncovered-p i pyramid-flags))
               (return-from invalidp t)))))
    (declare (inline invalidp))
    (let ((all-flags ()))
      (dotimes (pyramid-flags (ash 1 28) (nreverse all-flags))
        (unless (invalidp pyramid-flags)
          (push pyramid-flags all-flags))))))

(defvar *all-pyramid-flags* (all-pyramid-flags)
  "All valid PYRAMID-FLAGS values: no card removed unless it's uncovered.")

(defvar *all-uncovered-indexes*
  (loop for pyramid-flags in *all-pyramid-flags*
        collect (loop for i from 0 to 27
                      when (and (card-exists-p i pyramid-flags)
                                (card-uncovered-p i pyramid-flags))
                      collect i))
  "For each PYRAMID-FLAGS, the PYRAMID-INDEXes of its uncovered cards.")

(defvar *all-pyramid-indexes*
  (loop for pyramid-flags in *all-pyramid-flags*
        collect (loop for i from 0 to 27
                      when (card-exists-p i pyramid-flags)
                      collect i))
  "For each PYRAMID-FLAGS, the PYRAMID-INDEXes of all cards in the pyramid.")



;;; Functions and precalculated data for each deck of cards
(defun card-values (deck)
  "A vector containing the numeric rank of each card in DECK."
  (map 'vector #'value deck))

(defun card-bucket-masks (card-values)
  "For each card value (1 - 13), a mask of all DECK-INDEXes of that value.
So index 1 would be a mask with the bits for each Ace's position set to 1.
This is used to find out which Aces have not been removed yet."
  (let ((masks (make-array 14 :initial-element 0)))
    (dotimes (i 52 masks)
      (let ((card-value (svref card-values i)))
        (setf (svref masks card-value)
              (logior (svref masks card-value) (mask i)))))))

(deftype successor-masks ()
  '(simple-array list (53 52)))

(defun successor-masks (uncovered-indexes card-values)
  "Create all card removal masks for a given pyramid state.
UNCOVERED-INDEXES represents the uncovered cards in the pyramid state.
For every possible combination of stock-index and waste-index, this will
generate a list of card removal masks for the UNCOVERED-INDEXES.
Calling (aref [result] [stock-index] [waste-index]) gives you a list of masks
to remove any cards that can be removed from the given pyramid cards and
stock/waste cards.  This makes finding successor states for each state
simple and fast because it's all been precalculated ahead of time.
The simple way to implement this would be to just gather stock index + waste
index + uncovered indexes into one list and iteratively call GET-MASKS on each
sublist, but for speed this caches lists of masks for just removing cards
within the pyramid and ones where you remove a stock card with one from the
pyramid, or by itself if it's a king."
  (declare (optimize speed (safety 0) (debug 1))
           ((simple-vector 52) card-values))
  (labels ((kingp (index)
             "Return T if the card at INDEX is a King."
             (declare (deck-index index))
             (= 13 (the fixnum (svref card-values index))))
           (king-mask (index)
             "Return a mask to remove the card if it's a King, or NIL."
             (declare (deck-index index))
             (when (kingp index)
               (removal-mask index)))
           (matchp (index1 index2)
             "Return T if the cards at INDEX1 and INDEX2 add up to 13."
             (declare (deck-index index1 index2))
             (= 13 (the fixnum (+ (the fixnum (svref card-values index1))
                                  (the fixnum (svref card-values index2))))))
           (card-mask (index1 index2)
             "Return a mask for removing both cards if they add to 13, or NIL."
             (declare (deck-index index1 index2))
             (when (matchp index1 index2)
               (logand (removal-mask index1) (removal-mask index2))))
           (get-masks (deck-index other-indexes)
             "Return all removal masks using DECK-INDEX on OTHER-INDEXES."
             (declare (deck-index deck-index))
             (let ((king-mask (king-mask deck-index)))
               (if king-mask
                   (list king-mask)
                 (loop for i in other-indexes
                       when (card-mask deck-index i)
                       collect it))))
           (pyramid-masks ()
             "Cache all removal masks involving any cards in UNCOVERED-INDEXES."
             (loop for (i . rest) on uncovered-indexes
                   nconc (get-masks i rest)))
           (stock-masks ()
             "Cache all removal masks using a stock card on UNCOVERED-INDEXES."
             (loop with stock-masks = (make-array 54 :initial-element nil)
                   for i of-type stock-index from 28 to 51
                   do (setf (svref stock-masks i)
                            (get-masks i uncovered-indexes))
                   finally (return stock-masks))))
    (declare (inline kingp king-mask matchp card-mask get-masks))
    (loop with pyramid-masks = (pyramid-masks)
          with stock-masks = (stock-masks)
          with masks = (make-array '(53 52) :initial-element nil)
          for i of-type fixnum from 28 to 52
          for sp-masks = (append (svref stock-masks i) pyramid-masks)
          do (loop for j of-type fixnum from 27 below i
                   for sw-mask = (unless (or (stock-empty-p i)
                                             (waste-empty-p j))
                                   (card-mask i j))
                   for wsp-masks = (append (svref stock-masks j) sp-masks)
                   do (setf (aref masks i j)
                            (if sw-mask (cons sw-mask wsp-masks) wsp-masks)))
          finally (return masks))))

(defun h-cost (existing-pyramid-indexes card-values)
  "Given the cards in the pyramid, estimate how many steps to clear the board.
The estimate is the number of kings remaining in the pyramid, plus the
maximum count of the number of cards of each matching pair.  For example if
there are two fours and three nines in the pyramid, it would take at least
three steps to remove all the fours and nines."
  (loop with buckets = (make-array 14 :initial-element 0)
        for pyramid-index of-type pyramid-index in existing-pyramid-indexes
        do (incf (svref buckets (svref card-values pyramid-index)))
        finally (return (+ (svref buckets 13)
                           (max (svref buckets 1) (svref buckets 12))
                           (max (svref buckets 2) (svref buckets 11))
                           (max (svref buckets 3) (svref buckets 10))
                           (max (svref buckets 4) (svref buckets 9))
                           (max (svref buckets 5) (svref buckets 8))
                           (max (svref buckets 6) (svref buckets 7))))))

(defvar *unrelated-card-masks*
  #(#b1111111111111111111111110000000000000000000000000000
    #b1111111111111111111111111000000100000100001000100100
    #b1111111111111111111111110000001000001000010001001010
    #b1111111111111111111111111100000110000110001100110100
    #b1111111111111111111111111000001100001100011001101000
    #b1111111111111111111111110000011000011000110011011010
    #b1111111111111111111111111110000111000111001110110100
    #b1111111111111111111111111100001110001110011101100000
    #b1111111111111111111111111000011100011100111011001000
    #b1111111111111111111111110000111000111001110111011010
    #b1111111111111111111111111111000111100111101110110100
    #b1111111111111111111111111110001111001111011100100000
    #b1111111111111111111111111100011110011110111001000000
    #b1111111111111111111111111000111100111101110011001000
    #b1111111111111111111111110001111001111011110111011010
    #b1111111111111111111111111111100111110111101110110100
    #b1111111111111111111111111111001111101111001100100000
    #b1111111111111111111111111110011111011110011000000000
    #b1111111111111111111111111100111110111100110001000000
    #b1111111111111111111111111001111101111001110011001000
    #b1111111111111111111111110011111011111011110111011010
    #b1111111111111111111111111111110111110111101110110100
    #b1111111111111111111111111111101111100111001100100000
    #b1111111111111111111111111111011111001110001000000000
    #b1111111111111111111111111110111110011100010000000000
    #b1111111111111111111111111101111100111000110001000000
    #b1111111111111111111111111011111001111001110011001000
    #b1111111111111111111111110111111011111011110111011010)
  "A vector of masks to exclude the cards in DECK-FLAGS that are covering or
covered by the one at PYRAMID-INDEX, the index into the vector.  The nth card
in the pyramid can't be removed by a card that is masked off by the nth mask.")

(defun unwinnable-masks (pyramid-indexes card-values card-bucket-masks)
  "Return masks to check if a card in PYRAMID-INDEXES is unremovable.
This function locates the matching cards for each card in the pyramid and makes
a mask singling them out, then filters out the ones that are covering or 
covered by it."
  (let ((masks ()))
    (dolist (i pyramid-indexes masks)
      (let ((card-value (svref card-values i)))
        (unless (= card-value 13)
          (pushnew (logand (svref card-bucket-masks (- 13 card-value))
                           (svref *unrelated-card-masks* i))
                   masks))))))

(defstruct state-cache
  "SUCCESSOR-MASKS, H-COST, and UNWINNABLE-MASKS for a given PYRAMID-FLAGS."
  successor-masks
  h-cost
  unwinnable-masks)

(defun make-state-caches (deck)
  "Precalculate all the data we need to speed up processing for each state.
The result is a hash table indexed by one of 1430 PYRAMID-FLAGS, and the hash
value contains precalculated successor state masks (masks for removing cards
from each state), h-cost (A* heuristic function estimating how many steps to
reach the goal), and unwinnable masks (masks to determine if there exists a
card in the pyramid that can't be removed)."
  (loop with state-caches = (make-hash-table)
        with card-values = (card-values deck)
        with card-bucket-masks = (card-bucket-masks card-values)
        for pyramid-flags in *all-pyramid-flags*
        for uncovered-indexes in *all-uncovered-indexes*
        for all-indexes in *all-pyramid-indexes*
        do (setf (gethash pyramid-flags state-caches)
                 (make-state-cache
                  :successor-masks
                  (successor-masks uncovered-indexes card-values)
                  :h-cost
                  (h-cost all-indexes card-values)
                  :unwinnable-masks
                  (unwinnable-masks all-indexes card-values card-bucket-masks)))
        finally (return state-caches)))



;;; State related definitions
(deftype cycle ()
  "The player's current cycle through the stock cards."
  '(integer 1 3))

(deftype state ()
  "The game state (deck-flags / stock-index / cycle) combined into an integer.
Bits  0-51: DECK-FLAGS - indicates which cards in the deck remain.
Bits 52-57: STOCK-INDEX - index of the card at the top of the stock pile.
Bits 58-59: CYCLE - the current cycle through the stock/waste cards."
  '(unsigned-byte 60))

(declaim (inline state-goal-p))
(defun state-goal-p (state)
  "Return T if all 28 pyramid cards are removed from the state, NIL otherwise."
  (declare (optimize speed (safety 0))
           (type state state))
  (zerop (mask-field (byte 28 0) state)))

(declaim (inline state-h-cost))
(defun state-h-cost (state-cache)
  "Return an estimate of how many steps to clear all 28 pyramid cards."
  (declare (optimize speed (safety 0))
           (type state-cache state-cache))
  (state-cache-h-cost state-cache))

(declaim (inline state-unwinnable-p))
(defun state-unwinnable-p (state state-cache)
  "Return T if the state is definitely unwinnable, NIL otherwise.
NIL doesn't guarantee it's winnable.  This checks if there's a card on the
pyramid that can't be removed."
  (declare (optimize speed (safety 0))
           (type state state)
           (type state-cache state-cache))
  (dolist (mask (state-cache-unwinnable-masks state-cache))
    (declare (type deck-flags mask))
    (when (zerop (logand mask state))
      (return-from state-unwinnable-p t))))

(declaim (inline state-deck-flags))
(defun state-deck-flags (state)
  "Return the DECK-FLAGS stored inside STATE (bits 0-51)."
  (declare (type state state))
  (the deck-flags (mask-field (byte 52 0) state)))

(declaim (inline state-pyramid-flags))
(defun state-pyramid-flags (state)
  "Return the PYRAMID-FLAGS stored inside STATE (bits 0-27)."
  (declare (type state state))
  (the pyramid-flags (mask-field (byte 28 0) state)))

(declaim (inline state-stock-index))
(defun state-stock-index (state)
  "Return the STOCK-INDEX stored inside STATE (bits 52-57)."
  (declare (type state state))
  (the stock-index (ldb (byte 6 52) state)))

(declaim (inline state-cycle))
(defun state-cycle (state)
  "Return the CYCLE stored inside STATE (bits 58-59)."
  (declare (type state state))
  (the cycle (ldb (byte 2 58) state)))

(declaim (inline state-waste-index))
(defun state-waste-index (deck-flags stock-index)
  "Derive the WASTE-INDEX based on a STATE's DECK-FLAGS and STOCK-INDEX.
The top card of the waste pile is the next available card in the stock below
the STOCK-INDEX.  If the waste pile is empty, this function will return
+EMPTY-WASTE-INDEX+."
  (declare (type deck-flags deck-flags)
           (type stock-index stock-index))
  (do ((i (1- stock-index) (1- i)))
      ((or (waste-empty-p i) (card-exists-p i deck-flags)) i)
    (declare (type waste-index i))))

(declaim (inline make-state))
(defun make-state (deck-flags stock-index cycle)
  "Create a new state encapsulating DECK-FLAGS, STOCK-INDEX, and CYCLE."
  (declare (type deck-flags deck-flags)
           (type stock-index stock-index)
           (type cycle cycle))
  (do ((i stock-index (1+ i)))
      ((or (stock-empty-p i) (card-exists-p i deck-flags))
       (the state (logior deck-flags
                          (the state (ash i 52))
                          (the state (ash cycle 58)))))
    (declare (type stock-index i))))

(defvar +initial-state+ (make-state (1- (ash 1 52)) 28 1)
  "The initial state of every Pyramid Solitaire game, all cards in place.")

(defun state-successors (state state-cache)
  "Return a list of successor states for each applicable action from STATE."
  (declare (optimize speed (safety 0) (debug 0))
           (type state state)
           (type state-cache state-cache))
  (let* ((deck-flags (state-deck-flags state))
         (stock-index (state-stock-index state))
         (cycle (state-cycle state))
         (waste-index (state-waste-index deck-flags stock-index))
         (successor-masks (the successor-masks
                               (state-cache-successor-masks state-cache)))
         (successors ()))
    (if (stock-empty-p stock-index)
        (when (/= cycle 3)
          (push (make-state deck-flags 28 (1+ cycle)) successors))
      (push (make-state deck-flags (1+ stock-index) cycle) successors))
    (dolist (mask (aref successor-masks stock-index waste-index) successors)
      (declare (type deck-flags mask))
      (push (make-state (logand deck-flags mask) stock-index cycle)
            successors))))



;;; Search Node related definitions
(defun action (parent-state state deck)
  "Return a Lisp-readable action to get from PARENT-STATE to STATE."
  (let* ((state-diff (logxor state parent-state))
         (deck-flags-diff (mask-field (byte 52 0) state-diff))
         (cycle-diff (mask-field (byte 2 58) state-diff)))
    (cond ((not (eql 0 cycle-diff)) "Recycle")
          ((not (eql 0 deck-flags-diff))
           (loop for card in deck and i from 0
                 when (card-exists-p i deck-flags-diff)
                 collect card))
          (t "Draw"))))

(defun human-readable-action (action)
  "Return a human-readable string for the Lisp-readable ACTION."
  (cond ((equal action "Recycle")
         "Recycle the waste pile.")
        ((equal action "Draw")
         "Draw a card.")
        (t (format nil "Remove ~{~A~^ and ~}." action))))

(defun actions (node deck)
  "Return a list of Lisp-readable actions to go from the initial node to NODE."
  (loop for states on (reverse (rest node))
        for parent-state = (first states)
        for state = (second states)
        while state
        collect (action parent-state state deck)))



;;; Bucket Queue (Priority Queue) related definitions
(defstruct bucket-queue
  "A priority queue that can be used when priorities are small integers.
It's a vector of lists where items with priority N are inserted into index N
of the vector."
  (items (make-array 1 :initial-element ()) :type simple-vector)
  (front 1 :type fixnum))

(defun bucket-queue-add (bucket-queue item priority)
  "Add ITEM into the BUCKET-QUEUE with the given PRIORITY."
  (declare (optimize speed (safety 0))
           (type bucket-queue bucket-queue)
           (type fixnum priority))
  (when (< priority (bucket-queue-front bucket-queue))
    (setf (bucket-queue-front bucket-queue) priority))
  (push item (svref (bucket-queue-items bucket-queue) priority)))

(declaim (inline bucket-queue-empty-p))
(defun bucket-queue-empty-p (bucket-queue)
  "Return T if BUCKET-QUEUE has no items, NIL otherwise."
  (declare (optimize speed (safety 0))
           (type bucket-queue bucket-queue))
  (= (length (bucket-queue-items bucket-queue))
     (bucket-queue-front bucket-queue)))

(defun bucket-queue-remove (bucket-queue)
  "Remove and return the lowest priority item from BUCKET-QUEUE."
  (declare (optimize speed (safety 0))
           (type bucket-queue bucket-queue))
  (unless (bucket-queue-empty-p bucket-queue)
    (let* ((front (bucket-queue-front bucket-queue))
           (items (bucket-queue-items bucket-queue))
           (empty-queue-index (length items)))
      (prog1
          (pop (svref items front))
        (unless (svref items front)
          (setf (bucket-queue-front bucket-queue)
                (do ((i (1+ front) (1+ i)))
                    ((or (eql i empty-queue-index) (svref items i)) i)
                  (declare (type fixnum i)))))))))

(defun create-bucket-queue (max-priority)
  "Create a bucket queue for items of priority 0 to MAX-PRIORITY inclusive."
  (let ((size (1+ max-priority)))
    (make-bucket-queue :items (make-array size :initial-element nil)
                       :front size)))


(defun solve (deck)
  "A* solver for Pyramid Solitaire for the given DECK."
  (declare (optimize speed (safety 0) (debug 1)))
  (let* ((state-caches (make-state-caches deck))
         (fringe (create-bucket-queue 102))
         (seen-states (make-hash-table))
         (state +initial-state+)
         (node (cons 0 (list state)))
         (state-cache nil))
    (flet ((get-cache (state)
             (declare (type state state))
             (the state-cache (gethash (state-pyramid-flags state)
                                       state-caches))))
      (declare (inline get-cache))
      (setf state-cache (get-cache state))
      (unless (state-unwinnable-p state state-cache)
        (bucket-queue-add fringe node (state-h-cost state-cache)))
      (loop
       (when (bucket-queue-empty-p fringe) (return-from solve nil))
       (setf node (the cons (bucket-queue-remove fringe)))
       (setf state (the state (second node)))
       (when (state-goal-p state) (return-from solve (actions node deck)))
       (dolist (next-state (state-successors state (get-cache state)))
         (declare (type state next-state))
         (setf state-cache (get-cache next-state))
         (let ((seen-depth (gethash next-state seen-states))
               (next-depth (1+ (the (integer 0 100) (first node)))))
           (when (or (not seen-depth)
                     (< (the fixnum next-depth) (the fixnum seen-depth)))
             (setf (gethash next-state seen-states) next-depth)
             (unless (state-unwinnable-p next-state state-cache)
               (bucket-queue-add fringe
                                 (cons next-depth (cons next-state (rest node)))
                                 (+ next-depth
                                    (the (integer 0 28) (state-h-cost state-cache))))))))))))



;;; Testing functions
(defun run ()
  (let ((deck (string->deck "Th 2h 4d 3h Qd 8h 9h 5d Jc Td 7c 4c Ts Ac 9c 8d 5s 2s 7h 6s 7s 2c 9d Qs 3d 5c 5h Ad 8s Js 6c 9s 4h Kh Jd 4s 2d 6d Ks Qc 3s 3c Kc 7d Tc Ah 6h Qh Kd 8c As Jh")))
    (when (deckp deck)
      (solve deck))))

(defun run-decks (&optional (filename "resources/random-decks.txt"))
  (declare (optimize speed (safety 0) (debug 0)))
  (with-open-file (in filename)
    (loop for deck-string = (read-line in nil) and count of-type fixnum from 1
          while deck-string
          do (let* ((deck (string->deck deck-string))
                    (start-time (get-internal-real-time))
                    (solution (solve deck))
                    (total-time (- (get-internal-real-time) start-time)))
               (format t "~S~%" (list deck solution count total-time))))))

