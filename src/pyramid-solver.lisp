;;;; Find optimal length solutions for Pyramid Solitaire according to
;;;; Microsoft Solitaire Collection rules:
;;;; - 28 cards are on the table in a pyramid formation, and the other 24 are
;;;;   in a stack called the deck with only the top card showing.  There's also
;;;;   a waste pile which starts out empty.
;;;; - The goal is to remove all the 28 cards on the table.  The deck and waste
;;;;   pile can still have cards.
;;;; - Remove pairs of cards that add up to 13, or Kings by themselves because
;;;;   they already count as 13.  Jacks are 11, Queens are 12, Aces are 1.
;;;; - You can draw a card from the top of the deck onto the top of the waste
;;;;   pile.
;;;; - The only cards you can remove are the uncovered cards on the table, and
;;;;   the cards at the top of the deck and the top of the waste pile.
;;;; - You can cycle through the deck 3 times by recycling the waste pile.
;;;;   This means drawing all the cards on the waste pile back onto the deck so
;;;;   that you go through the deck in the same order again.  You can only
;;;;   recycle the waste pile twice.
;;;;
;;;; The solution will be a list of the following types of steps:
;;;; - Draw a card from the deck to the waste pile
;;;; - Recycle the waste pile back into the deck
;;;; - Remove a king by itself
;;;; - Remove a pair of cards that add up to 13

(in-package #:pyramid-solver)



;;; Card and Deck definitions
;;; Cards are two-letter strings of rank (A23456789TJQK) and suit (cdhs),
;;; for example "Ks" or "7c".
;;; Decks are vectors of 52 cards.
;;; Performance isn't really important for these since we'll only use them
;;; to precalculate things we want to know before starting the solver.

(defun card-rank (card)
  "Return CARD's rank as a character."
  (schar card 0))

(defun card-king-p (card)
  "Return T if the card is a King."
  (char= (card-rank card) #\K))

(defun card-bucket (card)
  "Return a numeric value for CARD's rank for use when bucketing cards by rank."
  (position (card-rank card) "KA23456789TJQ"))

(defun cards-match-p (card1 card2)
  "Return T if CARD1 and CARD2 are a pair that add up to 13."
  (= 13 (+ (card-bucket card1) (card-bucket card2))))

(defun card-match-table (deck)
  "Create an array of arrays of booleans indicating which cards match together."
  ;; the usage is taking one card and finding which cards out of a list of
  ;; other cards match with it, so for performance we use arrays of arrays
  ;; instead of a 2D array
  (let ((table (make-array 52)))
    (dotimes (index1 52 table)
      (setf (svref table index1) (make-array 52))
      (dotimes (index2 52)
        (setf (svref (svref table index1) index2)
              (cards-match-p (svref deck index1) (svref deck index2)))))))



;;; Table definitions
;;; Out of the 52 card deck, the first 28 are part of the pyramid on the table
;;; and the remaining 24 are deck/waste cards.
;;; The 28 cards on the table are indexed like this:
;;;             00
;;;           01  02
;;;         03  04  05
;;;       06  07  08  09
;;;     10  11  12  13  14
;;;   15  16  17  18  19  20
;;; 21  22  23  24  25  26  27
;;; 
;;; Some of the following definitions are just numbers because the values
;;; themselves are easier to understand visually than the code to generate them.
;;;
;;; The masks involve 52 bits where the nth bit being zero means the
;;; nth card in the deck was removed.  Bits 0-27 are the 28 table cards and
;;; bits 28-51 are the deck/waste cards.

(defvar *table-first-child-index*
  #(1 3 4 6 7 8 10 11 12 13 15 16 17 18 19 21 22 23 24 25 26)
  "For each table card index, the index of its first child card.
The second child is 1+ the first, but the bottom row indexes have no children.")

(defvar *table-unrelated-indexes*
  #(()
    (2 5 9 14 20 27)
    (1 3 6 10 15 21)
    (2 4 5 8 9 13 14 19 20 26 27)
    (3 5 6 9 10 14 15 20 21 27)
    (1 3 4 6 7 10 11 15 16 21 22)
    (2 4 5 7 8 9 12 13 14 18 19 20 25 26 27)
    (5 6 8 9 10 13 14 15 19 20 21 26 27)
    (3 6 7 9 10 11 14 15 16 20 21 22 27)
    (1 3 4 6 7 8 10 11 12 15 16 17 21 22 23)
    (2 4 5 7 8 9 11 12 13 14 17 18 19 20 24 25 26 27)
    (5 8 9 10 12 13 14 15 18 19 20 21 25 26 27)
    (6 9 10 11 13 14 15 16 19 20 21 22 26 27)
    (3 6 7 10 11 12 14 15 16 17 20 21 22 23 27)
    (1 3 4 6 7 8 10 11 12 13 15 16 17 18 21 22 23 24)
    (2 4 5 7 8 9 11 12 13 14 16 17 18 19 20 23 24 25 26 27)
    (5 8 9 12 13 14 15 17 18 19 20 21 24 25 26 27)
    (9 10 13 14 15 16 18 19 20 21 22 25 26 27)
    (6 10 11 14 15 16 17 19 20 21 22 23 26 27)
    (3 6 7 10 11 12 15 16 17 18 20 21 22 23 24 27)
    (1 3 4 6 7 8 10 11 12 13 15 16 17 18 19 21 22 23 24 25)
    (2 4 5 7 8 9 11 12 13 14 16 17 18 19 20 22 23 24 25 26 27)
    (5 8 9 12 13 14 17 18 19 20 21 23 24 25 26 27)
    (9 13 14 15 18 19 20 21 22 24 25 26 27)
    (10 14 15 16 19 20 21 22 23 25 26 27)
    (6 10 11 15 16 17 20 21 22 23 24 26 27)
    (3 6 7 10 11 12 15 16 17 18 21 22 23 24 25 27)
    (1 3 4 6 7 8 10 11 12 13 15 16 17 18 19 21 22 23 24 25 26))
  "The table cards that aren't covering/covered by the table card index.")

;;; The idea behind *TABLE-UNCOVERED-MASKS* and UNWINNABLE-MASKS is that
;;; we want to test for two things:
;;; 1) The table card exists (hasn't been removed).
;;; 2) The other cards in the mask don't exist (have been removed).
;;; If these two are true then:
;;; - For *TABLE-UNCOVERED-MASKS* the table card is there and its children are
;;;   not, meaning it's uncovered and a candidate for removal if there's a
;;;   matching card.
;;; - For UNWINNABLE-MASKS the table card is there but any potential matching
;;;   card to remove it with is gone, meaning the game is unwinnable.

(defvar *table-uncovered-masks*
  (apply #'vector
         (loop for i from 0 to 27
               for card-mask = (ash 1 i)
               if (< i 21)
               collect (logior card-mask
                               (ash #b11 (svref *table-first-child-index* i)))
               else
               collect card-mask))
  "Bit masks for the table cards to test if the card is uncovered.")

(defun unwinnable-masks (deck)
  "Return bit masks for the table cards to test if the card can't be removed."
  (let ((masks (make-array 28)))
    (dotimes (card-index 28 masks)
      (let ((mask 0)
            (card (svref deck card-index))
            (unrelated-indexes (svref *table-unrelated-indexes* card-index)))
        (unless (card-king-p card) ; kings are always removable so let it be 0
          (setf mask (ash 1 card-index))
          (dotimes (i 52)
            (when (and (or (> i 27)
                          (find i unrelated-indexes))
                      (cards-match-p card (svref deck i)))
              (setf mask (logior mask (ash 1 i))))))
        (setf (svref masks card-index) mask)))))



;;; Current Deck definitions
;;; These special variables below contain precalculated data for a given card
;;; deck to help the solver go faster.  The Nth index of each variable
;;; contains info on the Nth card in the deck.
(defvar *cards* nil "The cards in the deck")
(defvar *kings* nil "Which cards in the deck are kings")
(defvar *card-buckets* nil "The bucket number of each card in the deck")
(defvar *unwinnable-masks* nil "The unwinnable mask for each card")
(defvar *card-matches* nil "Which cards match with each card")

(defmacro with-deck (deck &body body)
  `(let* ((*cards* ,deck)
          (*kings* (map 'vector #'card-king-p *cards*))
          (*card-buckets* (map 'vector #'card-bucket *cards*))
          (*unwinnable-masks* (unwinnable-masks *cards*))
          (*card-matches* (card-match-table *cards*)))
     (declare (type simple-vector *cards*))
     ,@body))



;;; State definitions
;;; The "state of the world" of each step while playing Pyramid Solitaire.
;;;
;;; The state is represented as a 60-bit value:
;;; - Bits 0-51 represent the existence of each card in the deck
;;;   - Bits 0-27 are the 28 table cards
;;;   - Bits 28-51 are the 24 deck/waste cards
;;; - Bits 52-57 is a 6-bit number from 28-52 to indicate the deck index
;;;   - The card at the deck index is the top of the deck.
;;;   - Cards with index above the deck index are the rest of the deck cards.
;;;   - Cards with index below the deck index are the waste pile, the closest
;;;     one to the deck index is the top of the waste pile.
;;;   - Deck index value 52 means the deck is empty.
;;;   - Waste index value 27 means the waste pile is empty.
;;; - Bits 58-60 is a 2-bit number indicating which cycle through the deck
;;;   the player is on.  It can be a number from 1-3 inclusive.
;;;
;;; The main functions here are:
;;; - STATE-GOAL-P: did we win the game?
;;; - STATE-H-COST: estimate for A* how many more steps to win
;;; - STATE-UNWINNABLE-P: is the game now unwinnable?
;;; - STATE-SUCCESSORS: what actions can you take from this state?

(deftype state ()
  '(unsigned-byte 60))

(deftype exist-flags ()
  '(unsigned-byte 52))

(deftype deck-index ()
  '(integer 28 52)) ; 52 = empty deck

(deftype waste-index ()
  '(integer 27 51)) ; 27 = empty waste pile

(deftype cycle ()
  '(integer 1 3))

(deftype card-index ()
  '(integer 0 51))

(declaim (inline card-exists-p))
(defun card-exists-p (exist-flags card-index)
  "Return T if the card at CARD-INDEX has not been removed, NIL otherwise."
  (declare (optimize (speed 3) (safety 0))
           (type exist-flags exist-flags)
           (type card-index card-index))
  (not (eql 0 (logand exist-flags (the exist-flags (ash 1 card-index))))))

(declaim (inline deck-empty-p))
(defun deck-empty-p (deck-index)
  "Return T if the DECK-INDEX indicates the deck is empty."
  (declare (optimize (speed 3) (safety 0))
           (type deck-index deck-index))
  (= deck-index 52))

(declaim (inline waste-empty-p))
(defun waste-empty-p (waste-index)
  "Return T if the WASTE-INDEX indicates the waste pile is empty."
  (declare (optimize (speed 3) (safety 0))
           (type waste-index waste-index))
  (= waste-index 27))

(defun make-state (exist-flags deck-index cycle)
  "Return a new state encapsulating the EXIST-FLAGS, DECK-INDEX, and CYCLE."
  (declare (optimize (speed 3) (safety 0))
           (type exist-flags exist-flags)
           (type deck-index deck-index)
           (type cycle cycle))
  (do ((index deck-index (1+ index)))
      ((or (deck-empty-p index) (card-exists-p exist-flags index))
       (logior exist-flags
               (the (unsigned-byte 58) (ash index 52))
               (the (unsigned-byte 60) (ash cycle 58))))
    (declare (type deck-index index))))

(defvar *initial-state*
  (make-state (mask-field (byte 52 0) (lognot 0)) 28 1)
  "The initial state at the start of the game, no cards are removed yet.")

(defun state-fields (state)
  "Return the fields in STATE (exist-flags, deck-index, waste-index, cycle)."
  (declare (optimize (speed 3) (safety 0))
           (type state state))
  (let ((exist-flags (mask-field (byte 52 0) state))
        (deck-index (ldb (byte 6 52) state))
        (cycle (ldb (byte 2 58) state)))
    (declare (type exist-flags exist-flags)
             (type deck-index deck-index)
             (type cycle cycle))
    (do ((waste-index (1- deck-index) (1- waste-index)))
        ((or (waste-empty-p waste-index)
             (card-exists-p exist-flags waste-index))
         (values exist-flags deck-index waste-index cycle))
      (declare (type waste-index waste-index)))))

(defun state-goal-p (state)
  "Return T if the state has all the 28 table cards removed, NIL otherwise."
  (declare (optimize (speed 3) (safety 0))
           (type state state))
  (eql 0 (mask-field (byte 28 0) state)))

(defun state-h-cost (state)
  "Return an estimate of how many steps to reach the goal (clear the table)."
  (declare (optimize (speed 3) (safety 0))
           (type state state))
  (let ((buckets (make-array 13 :initial-element 0))
        (mask 1))
    (declare (type (unsigned-byte 28) mask))
    (dotimes (i 28)
      (unless (eql 0 (logand mask state))
        (let ((bucket (svref *card-buckets* i)))
          (setf (svref buckets bucket)
                (1+ (the (integer 0 3) (svref buckets bucket))))))
      (setf mask (ash mask 1)))
    (do ((i 1 (1+ i))
         (cost (svref buckets 0)
               (the (integer 0 28)
                    (+ cost 
                       (max (the (integer 0 4) (svref buckets i))
                            (the (integer 0 4) (svref buckets (- 13 i))))))))
         ((= i 7) cost)
      (declare (type (integer 1 7) i)
               (type (integer 0 28) cost)))))

(declaim (inline state-only-card-exists-p))
(defun state-only-card-exists-p (state table-index masks)
  "Return T if the TABLE-INDEX card exists, but the other cards are gone."
  (declare (optimize (speed 3) (safety 0))
           (type state state)
           (type (integer 0 27) table-index))
  (let ((card-exists-bit (ash 1 table-index))
        (mask (svref masks table-index)))
    (declare (type (unsigned-byte 28) card-exists-bit)
             (type exist-flags mask))
    (eql card-exists-bit (logand state mask))))

(defun state-unwinnable-p (state)
  "Return T if the state is definitely unwinnable, NIL otherwise.
NIL doesn't guarantee it's winnable however."
  (declare (optimize (speed 3) (safety 0))
           (type state state))
  (dotimes (i 28)
    (when (state-only-card-exists-p state i *unwinnable-masks*)
      (return-from state-unwinnable-p t))))

(defun state-uncovered-table-indexes (state)
  "Return a list of the table indexes that have uncovered cards."
  (declare (optimize (speed 3) (safety 0))
           (type state state))
  (let ((uncovered-indexes '()))
    (dotimes (i 28 uncovered-indexes)
      (when (state-only-card-exists-p state i *table-uncovered-masks*)
        (push i uncovered-indexes)))))

(defun state-successors (state)
  "Return a list of successor states for each applicable action from STATE."
  (declare (optimize (speed 3) (safety 0))
           (type state state))
  (multiple-value-bind (exist-flags deck-index waste-index cycle)
      (state-fields state)
    (declare (type exist-flags exist-flags)
             (type deck-index deck-index)
             (type waste-index waste-index)
             (type cycle cycle))
    (let ((uncovered-indexes (state-uncovered-table-indexes state))
          (successors '()))
      (labels ((add (exist-flags deck-index cycle)
                 (push (make-state exist-flags deck-index cycle) successors))
               (recycle ()
                 (add exist-flags 28 (1+ cycle)))
               (draw ()
                 (add exist-flags (1+ deck-index) cycle))
               (remove-king (index)
                 (let ((mask (ash 1 index)))
                   (declare (type exist-flags mask))
                   (add (logandc1 mask exist-flags) deck-index cycle)))
               (remove-pair (index1 index2)
                 (let* ((bit1 (ash 1 index1))
                        (bit2 (ash 1 index2))
                        (mask (logior bit1 bit2)))
                   (declare (type exist-flags bit1 bit2 mask))
                   (add (logandc1 mask exist-flags) deck-index cycle))))
        (declare (inline add recycle draw remove-king remove-pair))
        (if (deck-empty-p deck-index)
            (when (/= cycle 3)
              (recycle))
          (progn
            (push deck-index uncovered-indexes)
            (draw)))
        (unless (waste-empty-p waste-index)
          (push waste-index uncovered-indexes))
        (loop for indexes on uncovered-indexes
              for index1 of-type card-index = (first indexes)
              for index1-matches = (svref *card-matches* index1)
              do (if (svref *kings* index1)
                     (remove-king index1)
                   (loop for index2 of-type card-index in (rest indexes)
                         do (when (svref index1-matches index2)
                              (remove-pair index1 index2))))))
      successors)))
      
        

;;; Search Node definitions
;;; To save on memory there isn't a search node class or struct.  Instead,
;;; nodes are really just a list where the car is the depth and the cdr is the
;;; list of states starting from the current state and going backwards to the
;;; initial state. So:
;;; node's state = (second node)
;;; node's parent state = (third node)
;;; node's action = derive from the diff between parent state and state
;;; node's depth = (first node)
;;;
;;; Actions can be one of 3 things:
;;; 1) "Draw": Draw a card from the deck to the waste pile
;;; 2) "Recycle": Recycle the waste pile back into the deck
;;; 3) A list of cards to remove

(defun action (parent-state state)
  "Return a Lisp-readable action to get from PARENT-STATE to STATE."
  (let* ((diff (logxor state parent-state))
         (exist-flags-diff (mask-field (byte 52 0) diff))
         (cycle-diff (ldb (byte 2 58) diff)))
    (cond ((not (eql 0 cycle-diff)) "Recycle")
          ((not (eql 0 exist-flags-diff))
           (loop for i from 0 to 51
                 for flag = 1 then (ash flag 1)
                 unless (eql 0 (logand flag exist-flags-diff))
                 collect (svref *cards* i)))
          (t "Draw"))))

(defun human-readable-action (action)
  "Return a human-readable string for the Lisp-readable ACTION."
  (cond ((equal action "Recycle")
         "Recycle the waste pile.")
        ((equal action "Draw")
         "Draw a card.")
        ((= (length action) 1)
         (format nil "Remove ~A." (elt action 0)))
        (t
         (format nil "Remove ~A and ~A." (elt action 0) (elt action 1)))))

(defun actions (node)
  "Return a list of Lisp-readable actions to go from the initial node to NODE."
  (loop for states on (reverse (rest node))
        for parent-state = (first states)
        for state = (second states)
        while (and state parent-state)
        collect (action parent-state state)))



;;; Fringe definitions
;;; The fringe, or frontier, holds search nodes to be visited for A* search.
;;; This fringe is hardcoded to only allow priority 0 to 100 inclusive.
;;; Because of this, and because the only functionality we need to support is
;;; to add/remove/check if empty, we can use a bucket queue instead of a
;;; general purpose priority queue for speed.  There isn't functionality to
;;; replace an existing fringe node with a new one (in case you reach the same
;;; state with a shorter path) because it seems to be slower to do this work
;;; compared to letting the old nodes stay.
(defclass fringe ()
  ((items :accessor fringe-items
          :initform (make-array 101 :initial-element nil)
          :type (simple-vector 101)
          :documentation "A vector of lists of nodes indexed by priority.")
   (front :accessor fringe-front
          :initform 101
          :type (integer 0 101)
          :documentation "The bucket index with the lowest priority item.")
   (count :accessor fringe-count
          :initform 0
          :type fixnum
          :documentation "The total number of items in the fringe."))
  (:documentation "A bucket queue for priorities from 0 - 100.
This is a fast priority queue when you know the priorities are a small range.
It's an array of buckets, index N of the array holds a list of items with
priority N."))

(defun make-fringe ()
  "Return a new empty fringe."
  (make-instance 'fringe))

(defun fringe-add (fringe node priority)
  "Add NODE to the FRINGE with the given PRIORITY."
  (declare (optimize (speed 3) (safety 0)))
  (incf (the fixnum (fringe-count fringe)))
  (push node (svref (fringe-items fringe) priority))
  (when (< priority (the fixnum (fringe-front fringe)))
    (setf (fringe-front fringe) priority)))

(declaim (inline fringe-empty-p))
(defun fringe-empty-p (fringe)
  "Return T if FRINGE is empty, NIL otherwise."
  (declare (optimize (speed 3) (safety 0)))
  (zerop (the fixnum (fringe-count fringe))))

(defun fringe-remove (fringe)
  "Remove and return the lowest priority node in FRINGE."
  (declare (optimize (speed 3) (safety 0)))
  (unless (fringe-empty-p fringe)
    (with-accessors ((items fringe-items) (front fringe-front)) fringe
      (declare (type (integer 0 100) front))
      (prog1
          (pop (svref items front))
        (decf (the fixnum (fringe-count fringe)))
        (when (null (svref items front))
          (setf (fringe-front fringe)
                (if (fringe-empty-p fringe)
                    101
                  (loop for i from (1+ front) to 100
                        unless (null (svref items i))
                        return i))))))))



(defun collect-card-chars (deck-string)
  "Return a list of card rank and suit characters in DECK-STRING."
  (with-input-from-string (in deck-string)
    (loop for ch = (read-char in nil)
          while ch
          when (or (find (char-downcase ch) "cdhs")
                   (find (char-upcase ch) "A23456789TJQK"))
          collect it)))

(defun deck-string->card-list (deck-string)
  "Convert a string containing two-letter cards into a list of cards."
  (let ((card-chars (collect-card-chars deck-string)))
    (loop for (rank suit) on card-chars by #'cddr
          collect (format nil "~A~A" rank suit))))

(defun find-malformed-card (deck-string)
  "Return the first card that doesn't look like a card."
  (dolist (card (deck-string->card-list deck-string))
    (unless (and (= 2 (length card))
                 (and (find (aref card 0) "A23456789TJQK")
                      (find (aref card 1) "cdhs")))
      (return-from find-malformed-card card))))

(defun missing-cards (deck-string)
  "Return the cards from the 52-card deck that are missing from DECK-STRING."
  (let ((card-list (deck-string->card-list deck-string)))
    (apply #'append
           (loop for suit across "cdhs"
                 collect (loop for rank across "A23456789TJQK"
                               for card = (format nil "~A~A" rank suit)
                               unless (find card card-list :test #'equal)
                               collect card)))))

(defun num-cards (deck-string)
  "Return the number of cards in DECK-STRING."
  (length (deck-string->card-list deck-string)))

;;; A* solver for Pyramid Solitaire
(defun solve (deck-string)
  "Given a 52 card deck, set up Pyramid Solitaire and return a solution or NIL."
  (declare (optimize (speed 3) (safety 0)))
  (with-deck (apply #'vector (deck-string->card-list deck-string))
    (let* ((fringe (make-fringe))
           (seen-states (make-hash-table :test #'eql))
           (state *initial-state*)
           (node (cons 0 (list state))))
      (unless (state-unwinnable-p state)
        (fringe-add fringe node (state-h-cost state)))
      (loop
       (when (fringe-empty-p fringe) (return-from solve nil))
       (setf node (fringe-remove fringe))
       (setf state (second node))
       (when (state-goal-p state) (return-from solve (actions node)))
       (loop with next-depth of-type (integer 0 100) = (1+ (first node))
             for next-state in (state-successors state)
             for seen-depth of-type (or null (integer 0 100)) =
                 (gethash next-state seen-states)
             when (or (not seen-depth) (< next-depth seen-depth))
             do
             (setf (gethash next-state seen-states) next-depth)
             (unless (state-unwinnable-p next-state)
               (fringe-add fringe
                           (cons next-depth (cons next-state (rest node)))
                           (+ next-depth
			      (the (integer 0 100)
				   (state-h-cost next-state))))))))))

(defun run-decks (&optional (filename "resources/random-decks.txt"))
  (declare (optimize (speed 3) (safety 0)))
  (let ((start-time 0)
        (solution nil)
        (total-time 0))
    (with-open-file (in filename)
      (loop for deck-string = (read-line in nil)
            for deck-counter of-type fixnum from 1
            while deck-string
            do (progn
                 (setf start-time (get-internal-real-time))
                 (setf solution (solve deck-string))
                 (setf total-time (- (get-internal-real-time) start-time))
                 (format t "~S~%" (list (deck-string->card-list deck-string)
                                        solution
                                        deck-counter
                                        total-time)))))))
