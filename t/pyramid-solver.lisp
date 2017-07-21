(defpackage #:pyramid-solver-tests
  (:use #:cl #:fiveam #:ps))
(in-package #:pyramid-solver-tests)

(def-suite pyramid-solver-tests)
(in-suite pyramid-solver-tests)

(defparameter *all-cards*
  (loop for suit across "cdhs"
        nconc (loop for rank across "A23456789TJQK"
                    collect (ps::make-card rank suit))))

(defparameter *deck*
  (ps::string->deck (format nil "~A" *all-cards*)))

(defparameter *full-state*
  (logior #xfffffffffffff (ash 28 52) (ash 1 58))
  "The initial state where no cards are removed yet.")

(defparameter *empty-state*
  (logior (ash 28 52) (ash 1 58))
  "A state where all the cards are removed.")

(test all-cards-in-standard-deck-are-cards
  (finishes
    (dolist (card *all-cards*)
      (check-type card ps::card))))

(test non-cards-arent-cards
  (dolist (not-a-card '("Hc" ; bad rank
                        "Ka" ; bad suit
                        "A" ; too short
                        "Ac3" ; too long
                        (cons #\A #\c) ; not a string
                        #\A)) ; still not a string
    (signals (simple-type-error "~A is a card" (not-a-card))
      (check-type not-a-card ps::card))))

(test card-rank
  (dolist (card *all-cards*)
    (is (char= (schar card 0) (ps::rank card))
        "~A's rank should not be ~A" card (ps::rank card))))

(test card-kingp
  (is (null (set-difference (remove-if (complement #'ps::kingp) *all-cards*)
                            '("Kc" "Kd" "Kh" "Ks")
                            :test #'string=))))

(defun card-value (card)
  (ecase (schar card 0)
    (#\A 1)
    (#\2 2)
    (#\3 3)
    (#\4 4)
    (#\5 5)
    (#\6 6)
    (#\7 7)
    (#\8 8)
    (#\9 9)
    (#\T 10)
    (#\J 11)
    (#\Q 12)
    (#\K 13)))

(test card-value
  (flet ((values-match-p (card)
           (is (eql (card-value card) (ps::value card))
               "~A's value was returned as ~A" card (ps::value card))))
    (mapc #'values-match-p *all-cards*)))

(defun card-match-p (card1 card2)
  (member (sort (format nil "~C~C" (schar card1 0) (schar card2 0)) #'char<)
          '("AQ" "2J" "3T" "49" "58" "67")
          :test #'string=))

(test card-matchp
  (dolist (card1 *all-cards*)
    (dolist (card2 *all-cards*)
      (if (card-match-p card1 card2)
          (is-true (ps::matchp card1 card2)
                   "~A and ~A should match but don't" card1 card2)
        (is-false (ps::matchp card1 card2)
                  "~A and ~A should not match but do" card1 card2)))))

(test string->deck
  (let ((deck (ps::string->deck (format nil "~A" *all-cards*))))
    (finishes
     (check-type deck ps::deck))))

(test deckp-missing-cards
  (multiple-value-bind (is-deck num-cards missing duplicates malformed)
      (ps::deckp (ps::string->deck (format nil "~A" (rest *all-cards*))))
    (is-false is-deck)
    (is (= 51 num-cards))
    (is (equal '("Ac") missing))
    (is (null duplicates))
    (is (null malformed))))

(test deckp-duplicate-cards
  (multiple-value-bind (is-deck num-cards missing duplicates malformed)
      (ps::deckp (ps::string->deck (format nil "~A Kc" *all-cards*)))
    (is-false is-deck)
    (is (= 53 num-cards))
    (is (null missing))
    (is (equal '("Kc" "Kc") duplicates))
    (is (null malformed))))

(test deckp-malformed-cards
  (multiple-value-bind (is-deck num-cards missing duplicates malformed)
      (ps::deckp (ps::string->deck (format nil "~A d3" *all-cards*)))
    (is-false is-deck)
    (is (= 53 num-cards))
    (is (null missing))
    (is (null duplicates))
    (is (equal '("d3") malformed))))

(test mask
  (dotimes (i 52)
    (is (logbitp i (ps::mask i)))))

(test invert
  (dotimes (i 52)
    (is (= (1- (ash 1 52)) (logxor (ash 1 i) (ps::invert (ash 1 i)))))))

(test card-exists-p
  (let ((missing-first-card (logand (ps::invert (ps::mask 0)) (1- (ash 1 52)))))
    (dotimes (i 52)
      (if (= i 0)
          (is-false (logbitp i missing-first-card))
        (is-true (logbitp i missing-first-card))))))

(test stock-empty-p
  (loop for stock-index from 28 to 51
        do (is-false (ps::stock-empty-p stock-index)))
  (is-true (ps::stock-empty-p 52)))

(test waste-empty-p
  (loop for waste-index from 28 to 51
        do (is-false (ps::waste-empty-p waste-index)))
  (is-true (ps::waste-empty-p 27)))

(defparameter *pyramid-indexes*
  #2A((00 02 05 09 14 20 27)
      (01 04 08 13 19 26 00)
      (03 07 12 18 25 00 00)
      (06 11 17 24 00 00 00)
      (10 16 23 00 00 00 00)
      (15 22 00 00 00 00 00)
      (21 00 00 00 00 00 00))
  "Pyramid Indexes arranged in the order they are in PYRAMID-INDEXES.
If you tilt your head to the left you can see the pyramid shape.")

(defun locate-pyramid-index (pyramid-index)
  "Return the row and column of *PYRAMID-INDEXES* that contains PYRAMID-INDEX.
The row and column will be returned in a list, or NIL if not found."
  (dotimes (row 7)
    (dotimes (column (- 7 row))
      (when (= (aref *pyramid-indexes* row column) pyramid-index)
        (return-from locate-pyramid-index (list row column))))))

(defun ancestorp (ancestor-pyramid-index descendant-pyramid-index)
  "Return T if ANCESTOR-PYRAMID-INDEX is covered by DESCENDANT-PYRAMID-INDEX."
  (let ((ancestor-location (locate-pyramid-index ancestor-pyramid-index))
        (descendant-location (locate-pyramid-index descendant-pyramid-index)))
    (and (<= (first ancestor-location) (first descendant-location))
         (<= (second ancestor-location) (second descendant-location))
         (/= ancestor-pyramid-index descendant-pyramid-index))))

(test pyramid-cover-masks
  (dotimes (pyramid-index 28)
    (let ((mask (svref ps::*pyramid-cover-masks* pyramid-index)))
      (dotimes (i 28)
        (if (logbitp i mask)
            (is-true (ancestorp pyramid-index i))
          (is-false (ancestorp pyramid-index i)))))))

(test card-uncovered-p
  (is-true (ps::card-uncovered-p 0 1))
  (is-false (ps::card-uncovered-p 0 (logior 1 (ash 1 27)))))

(defun descendants (pyramid-index)
  "Return a list of all descendant pyramid indexes of PYRAMID-INDEX."
  (let ((location (locate-pyramid-index pyramid-index))
        (descendants ()))
    (dotimes (row 7 descendants)
      (dotimes (column (- 7 row))
        (let ((index (aref *pyramid-indexes* row column)))
          (when (and (/= index pyramid-index)
                     (>= row (first location))
                     (>= column (second location)))
            (push index descendants)))))))

(defun valid-pyramid-flags-p (pyramid-flags)
  "Return T if the PYRAMID-FLAGS has no covered cards removed."
  (labels ((card-exists-p (pyramid-index)
             (logbitp pyramid-index pyramid-flags)))
    (dotimes (i 28 t)
      (when (and (not (card-exists-p i))
                 (some #'card-exists-p (descendants i)))
        (return-from valid-pyramid-flags-p nil)))))

(test all-pyramid-flags
  (is (= 1430 (length ps::*all-pyramid-flags*)))
  (dolist (pyramid-flags ps::*all-pyramid-flags*)
    (is (valid-pyramid-flags-p pyramid-flags))))

(defun uncovered-indexes (pyramid-flags)
  "Return a list of pyramid card indexes that aren't covered by other cards."
  (labels ((card-exists-p (pyramid-index)
             (logbitp pyramid-index pyramid-flags)))
    (loop for i from 0 to 27
          when (and (card-exists-p i)
                    (notany #'card-exists-p (descendants i)))
          collect i)))

(test all-uncovered-indexes
  (is (= 1430 (length ps::*all-uncovered-indexes*)))
  (loop for pyramid-flags in ps::*all-pyramid-flags*
        for uncovered-indexes in ps::*all-uncovered-indexes*
        do (is (equal (sort (uncovered-indexes pyramid-flags) #'<)
                      (sort uncovered-indexes #'<)))))

(defun all-pyramid-indexes (pyramid-flags)
  "Return a list of all pyramid card indexes that exist in PYRAMID-FLAGS."
  (loop for i from 0 to 27
        when (logbitp i pyramid-flags)
        collect i))

(test all-pyramid-indexes
  (is (= 1430 (length ps::*all-pyramid-indexes*)))
  (loop for pyramid-flags in ps::*all-pyramid-flags*
        for all-indexes in ps::*all-pyramid-indexes*
        do (is (equal (sort (all-pyramid-indexes pyramid-flags) #'<)
                      (sort all-indexes #'<)))))

(test card-values
  (loop with actual-values = (ps::card-values *deck*)
        for expected-value in (mapcar #'card-value *deck*)
        for actual-value across actual-values
        do (is (= expected-value actual-value))))

(test bucket-cards-by-value
  (let ((actual-buckets (ps::bucket-cards-by-value (ps::card-values *deck*))))
    (loop for bucket across actual-buckets and value from 0
          do (loop for deck-index in bucket
                   do (is (= value (card-value (elt *deck* deck-index))))))))

(test card-bucket-masks
  (let ((actual-masks (ps::card-bucket-masks
                       (ps::bucket-cards-by-value (ps::card-values *deck*))))
        (expected-mask #b0000000000001000000000000100000000000010000000000001))
    (loop for i from 1 to 13
          do (is (= (ash expected-mask (1- i)) (svref actual-masks i))))))

(defun make-king-mask (index)
  "Create a card removal mask to remove a king at INDEX."
  (mask-field (byte 52 0) (lognot (ash 1 index))))

(test king-masks
  (let ((king-masks (ps::king-masks (ps::card-values *deck*))))
    (dotimes (i 52)
      (is (eql (svref king-masks i)
               (case i
                 ((12 25 38 51) (make-king-mask i))
                 (otherwise nil)))))))

(defun matchp (card1 card2)
  "Return T if CARD1 and CARD2 are a match that can be removed together."
  (let ((card2-rank (schar card2 0)))
    (case (schar card1 0)
      (#\A (char= #\Q card2-rank))
      (#\2 (char= #\J card2-rank))
      (#\3 (char= #\T card2-rank))
      (#\4 (char= #\9 card2-rank))
      (#\5 (char= #\8 card2-rank))
      (#\6 (char= #\7 card2-rank))
      (#\7 (char= #\6 card2-rank))
      (#\8 (char= #\5 card2-rank))
      (#\9 (char= #\4 card2-rank))
      (#\T (char= #\3 card2-rank))
      (#\J (char= #\2 card2-rank))
      (#\Q (char= #\A card2-rank))
      (otherwise nil))))

(defun make-card-mask (index1 index2)
  "Create a card removal mask to remove INDEX1 and INDEX2."
  (mask-field (byte 52 0) (lognot (logior (ash 1 index1) (ash 1 index2)))))

(test card-masks
  (let* ((card-values (ps::card-values *deck*))
         (card-buckets (ps::bucket-cards-by-value card-values))
         (card-masks (ps::card-masks card-values card-buckets)))
    (dotimes (index1 52)
      (dotimes (index2 52)
        (is (eql (if (matchp (elt *deck* index1) (elt *deck* index2))
                     (make-card-mask index1 index2)
                   nil)
                 (aref card-masks index1 index2)))))))

(defun successor-masks (uncovered-indexes stock-index waste-index card-values)
  "Return a list of all card removal masks involving the given indexes/cards."
  (labels ((all-indexes ()
             "Combine all indexes into one list, ignoring empty stock/waste."
             (nconc (if (= stock-index 52) nil (list stock-index))
                    (if (= waste-index 27) nil (list waste-index))
                    uncovered-indexes))
           (masks (index other-indexes)
             "Create all card removal masks that involve the card at INDEX."
             (if (= 13 (svref card-values index))
                 (list (make-king-mask index))
               (loop with value = (svref card-values index)
                     for i in other-indexes
                     when (= 13 (+ value (svref card-values i)))
                     collect (make-card-mask index i)))))
    (loop for (i . rest) on (all-indexes)
          nconc (masks i rest))))

(test successor-masks
  (loop with start = 1200
        with end = 1220
        with card-values = (ps::card-values *deck*)
        with card-buckets = (ps::bucket-cards-by-value card-values)
        with kmasks = (ps::king-masks card-values)
        with cmasks = (ps::card-masks card-values card-buckets)
        for uncovered-indexes in (subseq ps::*all-uncovered-indexes* start end)
        for actual-masks = (ps::successor-masks uncovered-indexes kmasks cmasks)
        do (loop for i from 28 to 52
                 do (loop for j from 27 below i
                          for expected = (successor-masks uncovered-indexes
                                                          i j card-values)
                          for actual = (aref actual-masks i j)
                          do (is (and (null (set-difference expected actual))
                                      (null (set-difference actual expected)))
                                 "Mismatch: stock=~A waste=~A expected=~A actual=~A" i j expected actual)))))

(defun h-cost (existing-pyramid-indexes deck)
  "Estimate how many steps to remove the cards in EXISTING-PYRAMID-INDEXES."
  (labels ((value (pyramid-index)
             (card-value (elt deck pyramid-index)))
           (num-cards-of-rank (rank-value)
             (count rank-value existing-pyramid-indexes :key #'value)))
    (+ (num-cards-of-rank 13)
       (max (num-cards-of-rank 1) (num-cards-of-rank 12))
       (max (num-cards-of-rank 2) (num-cards-of-rank 11))
       (max (num-cards-of-rank 3) (num-cards-of-rank 10))
       (max (num-cards-of-rank 4) (num-cards-of-rank 9))
       (max (num-cards-of-rank 5) (num-cards-of-rank 8))
       (max (num-cards-of-rank 6) (num-cards-of-rank 7)))))

(test h-cost
  (loop with card-values = (map 'vector #'ps::value *deck*)
        for pyramid-indexes in ps::*all-pyramid-indexes*
        do (is (eql (h-cost pyramid-indexes *deck*)
                    (ps::h-cost pyramid-indexes card-values)))))

(defun unrelated-card-mask (pyramid-index)
  (flet ((unrelatedp (index1 index2)
           "Return T if INDEX2 isn't covering/covered by INDEX1."
           (not (or (= index1 index2)
                    (ancestorp index1 index2)
                    (ancestorp index2 index1)))))
    (let ((mask (ash #xffffff 28))) ; the 24 stock/waste cards are included
      (dotimes (i 28 mask)
        (when (unrelatedp pyramid-index i)
          (setf mask (logior mask (ash 1 i))))))))

(test unrelated-card-masks
  (loop for actual-mask across ps::*unrelated-card-masks* and i from 0
        for expected-mask = (unrelated-card-mask i)
        do (is (eql expected-mask actual-mask))))

(defun matching-card-mask (pyramid-index deck)
  "Return a mask of all cards that add to 13 with the card at PYRAMID-INDEX.
Ignore kings because they don't need a match to be removed."
  (loop with value = (card-value (elt deck pyramid-index))
        with mask = 0
        for card in deck and i from 0
        when (= 13 (+ value (card-value card)))
        do (setf mask (logior mask (ash 1 i)))
        finally (return mask)))

(defun unwinnable-masks (pyramid-indexes deck)
  "Return a list of masks to check if a card in the pyramid can't be removed.
For each card in PYRAMID-INDEXES, the mask has bits set for each card that can
be used to remove that card."
  (remove-duplicates (loop for i in pyramid-indexes
                           for mask = (matching-card-mask i deck)
                           unless (= mask 0) ; a king
                           collect (logand mask (unrelated-card-mask i)))))

(test unwinnable-masks
  (loop with card-values = (map 'vector #'ps::value *deck*)
        with card-buckets = (ps::bucket-cards-by-value card-values)
        with bucket-masks = (ps::card-bucket-masks card-buckets)
        for all-indexes in ps::*all-pyramid-indexes*
        for actual = (ps::unwinnable-masks all-indexes card-values bucket-masks)
        for expected = (unwinnable-masks all-indexes *deck*)
        do (is (and (null (set-difference actual expected))
                    (null (set-difference expected actual))))))

(test state-goal-p-empty-table
  (is-true (ps::state-goal-p *empty-state*)))

(test state-goal-p-full-table
  (is-false (ps::state-goal-p *full-state*)))

(test state-goal-p-just-one-table-card
  (let ((one-card-state (ps::make-state 1 28 1)))
    (is-false (ps::state-goal-p one-card-state))))

(defun get-state-cache (state deck)
  (gethash (ps::state-pyramid-flags state) (ps::make-state-caches deck)))

(test state-h-cost
  (is (= 16 (ps::state-h-cost (get-state-cache *full-state* *deck*)))))

(test state-h-cost-empty-table
  (is (= 0 (ps::state-h-cost (get-state-cache *empty-state* *deck*)))))

(test state-h-cost-partial-table
  (let ((state (logandc1 (ash #b11 25) *full-state*)))
    (is (= 14 (ps::state-h-cost (get-state-cache state *deck*))))))

(test state-unwinnable-p-with-unwinnable-state
  (let ((deck
         #("2d" "9s" "7c" "5d" "2s" "Qc" "Jd" "5c" "Jc" "Td" "4s" "6s" "8c"
           "8s" "Jh" "5h" "As" "Js" "6d" "2c" "Qd" "Qh" "4c" "8h" "Ks" "7d"
           "Ah" "4d" "9h" "3d" "5s" "4h" "Th" "Ad" "3s" "8d" "Ts" "Tc" "9d"
           "Kc" "7h" "Kd" "6h" "Qs" "2h" "Ac" "7s" "6c" "3c" "3h" "9c" "Kh")))
    (is-true (ps::state-unwinnable-p *full-state*
                                     (get-state-cache *full-state* deck)))))

(test state-unwinnable-p-with-winnable-state
  (let ((deck
         #("6d" "5h" "Ah" "Jd" "4s" "Ks" "6s" "8c" "2h" "4d" "9s" "Kd" "6c"
           "Ad" "8s" "Ac" "5c" "9d" "7h" "3h" "8d" "5s" "4c" "Qc" "Jh" "Kc"
           "Kh" "3c" "3s" "9c" "As" "5d" "Qh" "Ts" "4h" "7s" "Td" "9h" "Th"
           "7c" "8h" "2c" "7d" "Tc" "2d" "6h" "2s" "Js" "Qd" "3d" "Qs" "Jc")))
    (is-false (ps::state-unwinnable-p *full-state*
                                      (get-state-cache *full-state* deck)))))

;;; To perform full testing of successors we should have cases that:
;;; 1) Draw a card because there's cards on the deck.
;;; 2) Don't draw a card because the deck is empty.
;;; 3) Recycle the waste pile because the deck is empty.
;;; 4) Don't recycle because the deck isn't empty.
;;; 5) Don't recycle because we're on the 3rd cycle already.
;;; 6) Remove a King on the table.
;;; 7) Remove a King from the top of the deck.
;;; 8) Remove a King from the top of the waste pile.
;;; 9) Remove a pair of cards, both on the table.
;;; 10) Remove a pair of cards, one on the table and one on the deck.
;;; 11) Remove a pair of cards, one on the table and one on the waste pile.
;;; 12) Remove a pair of cards, one on the deck and one on the waste pile.
;;;
;;; *deck* formatted more nicely:
;;;             Ac
;;;           2c  3c
;;;         4c  5c  6c
;;;       7c  8c  9c  Tc
;;;     Jc  Qc  Kc  Ad  2d
;;;   3d  4d  5d  6d  7d  8d
;;; 9d  Td  Jd  Qd  Kd  Ah  2h
;;; 3h 4h 5h 6h 7h 8h 9h Th Jh Qh Kh As 2s 3s 4s 5s 6s 7s 8s 9s Ts Js Qs Ks

(defun make-state (deck-flags stock-index cycle)
  "Return a state created directly without moving deck-index to a valid value."
  (logior deck-flags (ash stock-index 52) (ash cycle 58)))

(defun remove-cards (deck deck-flags cards-to-remove)
  "Return DECK-FLAGS but with CARDS-TO-REMOVE in DECK removed from it."
  (dolist (card cards-to-remove deck-flags)
    (setf deck-flags (logandc1 (ash 1 (position card deck :test #'string=))
                               deck-flags))))

(defun expected-successors (state deck expected-data)
  "Gather expected successor (action . state) pairs using EXPECTED-DATA."
  (loop with old-deck-flags = (mask-field (byte 52 0) state)
        for (action stock-index cycle) in expected-data
        for deck-flags = (if (listp action)
                             (remove-cards deck old-deck-flags action)
                           old-deck-flags)
        collect (cons action (make-state deck-flags stock-index cycle))))

(defun actual-successors (state state-cache deck)
  "Gather successor (action . state) pairs for actions starting from STATE."
  (loop for next-state in (ps::state-successors state state-cache)
        collect (cons (ps::action state next-state deck) next-state)))

(defun successors-diffs (deck starting-state expected-data)
  "Compare expected and actual successors and test what's missing in each."
  (let ((state-cache (get-state-cache starting-state deck)))
    (let ((expected (expected-successors starting-state deck expected-data))
          (actual (actual-successors starting-state state-cache deck)))
      (is (null (set-difference expected actual :test #'equal)))
      (is (null (set-difference actual expected :test #'equal))))))

(test state-successors-draw
  (successors-diffs *deck* *full-state*
                    '(; case 4: can't recycle because the deck isn't empty
                      ("Draw" 29 1) ; case 1
                      (("Jd" "2h") 28 1) ; case 9
                      (("Qd" "Ah") 28 1) ; case 9
                      (("Kd") 28 1) ; case 6
                      (("Td" "3h") 29 1)))) ; case 10

(test state-successors-recycle
  (successors-diffs *deck* (make-state (1- (expt 2 52)) 52 1)
                    '( ; case 2: can't draw because deck is empty
                      ("Recycle" 28 2) ; case 3
                      (("Jd" "2h") 52 1) ; case 9
                      (("Qd" "Ah") 52 1) ; case 9
                      (("Kd") 52 1) ; case 6
                      (("Ks") 52 1)))) ; case 8
       
(test state-successors-end-of-cycle3
  (successors-diffs *deck* (make-state (1- (expt 2 52)) 52 3)
                    '( ; case 5: can't recycle because it's cycle 3
                      (("Jd" "2h") 52 3) ; case 9
                      (("Qd" "Ah") 52 3) ; case 9
                      (("Kd") 52 3) ; case 6
                      (("Ks") 52 3)))) ; case 8

(test state-successors-remove-king-from-deck
  (successors-diffs *deck* (make-state (1- (expt 2 52)) 51 1)
                    '(("Draw" 52 1) ; case 1
                      (("Jd" "2h") 51 1) ; case 9
                      (("Qd" "Ah") 51 1) ; case 9
                      (("Kd") 51 1) ; case 6
                      (("Ah" "Qs") 51 1) ; case 11
                      (("Ks") 52 1)))) ; case 7

(test state-successors-deck-and-waste-pair
  (successors-diffs *deck* (make-state (1- (expt 2 52)) 32 1)
                    '((("6h" "7h") 33 1) ; case 12
                      ("Draw" 33 1) ; case 1
                      (("Jd" "2h") 32 1) ; case 9
                      (("Qd" "Ah") 32 1) ; case 9
                      (("Kd") 32 1)))) ; case 6

(test new-fringe-is-empty
  (let ((fringe (ps::create-bucket-queue 100)))
    (is-true (ps::bucket-queue-empty-p fringe))))

(test fringe-add-remove-empty
  (let ((fringe (ps::create-bucket-queue 100)))
    (ps::bucket-queue-add fringe "one" 1)
    (is (string= "one" (ps::bucket-queue-remove fringe)))
    (is-true (ps::bucket-queue-empty-p fringe))))

(test fringe-remove-when-empty-returns-nil
  (let ((fringe (ps::create-bucket-queue 100)))
    (is (not (ps::bucket-queue-remove fringe)))))

(test fringe-add-remove-updates-front-correctly
  (let ((fringe (ps::create-bucket-queue 100)))
    (ps::bucket-queue-add fringe "twelve" 12)
    (ps::bucket-queue-add fringe "twelve-two" 12)
    (ps::bucket-queue-add fringe "fourteen" 14)
    (is (string= "twelve" (subseq (ps::bucket-queue-remove fringe) 0 6)))
    (is (string= "twelve" (subseq (ps::bucket-queue-remove fringe) 0 6)))
    (is (string= "fourteen" (ps::bucket-queue-remove fringe)))
    (is-true (ps::bucket-queue-empty-p fringe))
    (ps::bucket-queue-add fringe "hundred" 100)
    (is-false (ps::bucket-queue-empty-p fringe))))

(test solve-minimal
  (let* ((deck (ps::string->deck "
            Kd
          Kc  Qh
        Ah  7d  6d
      8d  5d  9d  4d
    Td  3d  Jd  2d  Qd
  Ad  7c  6c  8c  5c  9c
4c  Tc  3c  Jc  2c  Qc  Ac
6h 7h 5h 8h 4h 9h 3h Th 2h Jh Kh As 2s 3s 4s 5s 6s 7s 8s 9s Ts Js Qs Ks"))
         (solution (solve deck)))
    (is (= 15 (length solution)))))

(test solve-44-step-solution
  (let* ((deck (ps::string->deck "
            Th
          Js  Jh
        9c  Qd  5c
      2d  9h  Td  4h
    Qs  9d  3s  8d  Kh
  6c  3h  6d  8c  Kc  Ah
Qh  Tc  9s  Kd  8s  4s  2c
4c Jc 7c Jd 8h 6s 5d 3c 4d 3d 6h Ts 5s Ks 7d Ac 7s 2s Qc 2h 5h As 7h Ad"))
         (solution (ps::solve deck)))
    (is (= 44 (length solution)))))
