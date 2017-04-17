(defpackage #:pyramid-solver-tests
  (:use #:cl #:fiveam #:ps))
(in-package #:pyramid-solver-tests)

(def-suite pyramid-solver-tests)
(in-suite pyramid-solver-tests)

(defvar *deck*
  (apply #'vector (loop for suit across "cdhs"
		     append (loop for rank across "A23456789TJQK"
			       collect (format nil "~A~A" rank suit))))
  "All the cards in a 52-card deck.")

(defvar *shortest-solution-deck*
  "         Kd
          Kc  Qh
        Ah  7d  6d
      8d  5d  9d  4d
    Td  3d  Jd  2d  Qd
  Ad  7c  6c  8c  5c  9c
4c  Tc  3c  Jc  2c  Qc  Ac
6h 7h 5h 8h 4h 9h 3h Th 2h Jh Kh As 2s 3s 4s 5s 6s 7s 8s 9s Ts Js Qs Ks"
  "This deck should have a solution with 15 steps.")

(defvar *full-state*
  (logior (1- (expt 2 52)) (ash 28 52) (ash 1 58))
  "The initial state where no cards are removed yet.")

(defvar *empty-state*
  (logior (ash 28 52) (ash 1 58))
  "A state where all the cards are removed.")

(test card-king-p
  (loop for card across *deck*
       do (if (char= (schar card 0) #\K)
	      (is-true (ps::card-king-p card))
	      (is-false (ps::card-king-p card)))))

(test cards-match-p
  (flet ((ranks-match (rank1 rank2)
           (case rank1
             (#\A (char= rank2 #\Q))
             (#\2 (char= rank2 #\J))
             (#\3 (char= rank2 #\T))
             (#\4 (char= rank2 #\9))
             (#\5 (char= rank2 #\8))
             (#\6 (char= rank2 #\7))
             (#\7 (char= rank2 #\6))
             (#\8 (char= rank2 #\5))
             (#\9 (char= rank2 #\4))
             (#\T (char= rank2 #\3))
             (#\J (char= rank2 #\2))
             (#\Q (char= rank2 #\A))
             (otherwise nil))))
    (loop for card1 across *deck*
	 do (loop for card2 across *deck*
		 do (if (ranks-match (schar card1 0) (schar card2 0))
			(is-true (ps::cards-match-p card1 card2))
			(is-false (ps::cards-match-p card1 card2)))))))

(defun first-child-indexes ()
  "Generate a vector of each index's first child, for those that have one."
  ;; If you tilt your head you can see the pyramid in the 2D array below.
  ;; The first child is always the index one row below it.
  (let ((table (make-array '(7 7)
                           :initial-contents '((00 02 05 09 14 20 27)
                                               (01 04 08 13 19 26 00)
                                               (03 07 12 18 25 00 00)
                                               (06 11 17 24 00 00 00)
                                               (10 16 23 00 00 00 00)
                                               (15 22 00 00 00 00 00)
                                               (21 00 00 00 00 00 00))))
        (child-indexes (make-array 21)))
    (dotimes (row 7 child-indexes)
      (loop for column from 0 below (- 6 row) ; skip 21-27
            for index = (aref table row column)
            for child = (aref table (1+ row) column)
            do (setf (svref child-indexes index) child)))))

(test table-first-child-index
  (is (equalp (first-child-indexes) ps::*table-first-child-index*)))

(defun table-index-covering-p (index1 index2)
  "Return T if INDEX2 is covering INDEX1, either directly or indirectly.
Another way of saying it is return T if INDEX2 is a descendant of INDEX1."
  (unless (>= index1 21)
    (let* ((first-child-indexes (first-child-indexes))
           (first-child-index (svref first-child-indexes index1))
           (second-child-index (1+ first-child-index)))
      (or (= index2 first-child-index)
          (= index2 second-child-index)
          (table-index-covering-p first-child-index index2)
          (table-index-covering-p second-child-index index2)))))

(defun table-index-unrelated-p (index1 index2)
  "Return T if INDEX1 and INDEX2 are not covering each other."
  (not (or (= index1 index2)
           (table-index-covering-p index1 index2)
           (table-index-covering-p index2 index1))))

(defun table-unrelated-indexes ()
  "Generate a vector of each index's unrelated indexes.  In other words the
indexes that are neither covering nor covered by each other."
  (apply #'vector
         (loop for i from 0 to 27
               collect (loop for j from 0 to 27
                             when (table-index-unrelated-p i j)
                             collect j))))

(test table-unrelated-indexes
  (is (equalp (table-unrelated-indexes) ps::*table-unrelated-indexes*)))

(test new-fringe-is-empty
  (let ((fringe (ps::make-fringe)))
    (is-true (ps::fringe-empty-p fringe))))

(test fringe-add-remove-empty
  (let ((fringe (ps::make-fringe)))
    (ps::fringe-add fringe "one" 1)
    (is (string= "one" (ps::fringe-remove fringe)))
    (is-true (ps::fringe-empty-p fringe))))

(test fringe-remove-when-empty-returns-nil
  (let ((fringe (ps::make-fringe)))
    (is (not (ps::fringe-remove fringe)))))

(test fringe-add-remove-updates-front-correctly
  (let ((fringe (ps::make-fringe)))
    (ps::fringe-add fringe "twelve" 12)
    (ps::fringe-add fringe "twelve-two" 12)
    (ps::fringe-add fringe "fourteen" 14)
    (is (string= "twelve" (subseq (ps::fringe-remove fringe) 0 6)))
    (is (string= "twelve" (subseq (ps::fringe-remove fringe) 0 6)))
    (is (string= "fourteen" (ps::fringe-remove fringe)))
    (is-true (ps::fringe-empty-p fringe))
    (ps::fringe-add fringe "hundred" 100)
    (is-false (ps::fringe-empty-p fringe))))

(test state-goal-p-empty-table
  (ps::with-deck *deck*
    (is-true (ps::state-goal-p *empty-state*))))

(test state-goal-p-full-table
  (ps::with-deck *deck*
    (is-false (ps::state-goal-p *full-state*))))

(test state-goal-p-just-one-table-card
  (ps::with-deck *deck*
    (let ((one-card-state (ps::make-state 1 28 1)))
      (is-false (ps::state-goal-p one-card-state)))))

(test state-h-cost
  (ps::with-deck *deck*
    (is (= 16 (ps::state-h-cost *full-state*)))))

(test state-h-cost-empty-table
  (ps::with-deck *deck*
    (is-true (zerop (ps::state-h-cost *empty-state*)))))

(test state-h-cost-partial-table
  (ps::with-deck *deck*
    (is (= 14 (ps::state-h-cost (logandc1 (ash #b11 25) *full-state*))))))

(test state-unwinnable-p-with-unwinnable-state
  (ps::with-deck
      #("2d" "9s" "7c" "5d" "2s" "Qc" "Jd" "5c" "Jc" "Td" "4s" "6s" "8c"
        "8s" "Jh" "5h" "As" "Js" "6d" "2c" "Qd" "Qh" "4c" "8h" "Ks" "7d"
        "Ah" "4d" "9h" "3d" "5s" "4h" "Th" "Ad" "3s" "8d" "Ts" "Tc" "9d"
        "Kc" "7h" "Kd" "6h" "Qs" "2h" "Ac" "7s" "6c" "3c" "3h" "9c" "Kh")
    (is-true (ps::state-unwinnable-p *full-state*))))
    
(test state-unwinnable-p-with-winnable-state
  (ps::with-deck
      #("6d" "5h" "Ah" "Jd" "4s" "Ks" "6s" "8c" "2h" "4d" "9s" "Kd" "6c"
        "Ad" "8s" "Ac" "5c" "9d" "7h" "3h" "8d" "5s" "4c" "Qc" "Jh" "Kc"
        "Kh" "3c" "3s" "9c" "As" "5d" "Qh" "Ts" "4h" "7s" "Td" "9h" "Th"
        "7c" "8h" "2c" "7d" "Tc" "2d" "6h" "2s" "Js" "Qd" "3d" "Qs" "Jc")
    (is-false (ps::state-unwinnable-p *full-state*))))

(defun uncovered-match-p (expected state &rest indexes-to-remove)
  "Given a STATE with the cards at INDEXES-TO-REMOVE removed, check if the
uncovered table card indexes match EXPECTED."
  (dolist (index indexes-to-remove)
    (setf state (logandc1 (ash 1 index) state)))
  (null (set-difference expected (ps::state-uncovered-table-indexes state))))

(test state-uncovered-table-last-card
  (ps::with-deck *deck*
    (let ((state (logior #b111 (ash 28 52) (ash 1 58))))
      (is (uncovered-match-p '(0) state 1 2))
      (is (uncovered-match-p '(2) state 1))
      (is (uncovered-match-p '(1) state 2))
      (is (uncovered-match-p '(1 2) state))
      (is (uncovered-match-p '() state 0 1 2)))))

(test state-uncovered-table-bottom-row
  (ps::with-deck *deck*
    (is (uncovered-match-p '(21 22 23 24 25 26 27) *full-state*))
    (is (uncovered-match-p '(21 22 23 24 25 27) *full-state* 26))
    (is (uncovered-match-p '(21 22 23 24 25 26) *full-state* 27))
    (is (uncovered-match-p '(21 22 23 24 25 20) *full-state* 26 27))
    (is (uncovered-match-p '(21 22 23 24 25) *full-state* 20 26 27))))

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

(defun make-state (exist-flags deck-index cycle)
  "Return a state created directly without moving deck-index to a valid value."
  (logior exist-flags (ash deck-index 52) (ash cycle 58)))

(defun remove-cards (deck exist-flags &rest cards-to-remove)
  "Return EXIST-FLAGS where the CARDS-TO-REMOVE in DECK are removed."
  (dolist (card cards-to-remove exist-flags)
    (setf exist-flags
          (logandc1 (ash 1 (position card deck :test #'equal))
                    exist-flags))))

(defun expected-successors (deck starting-state expected-data)
  "Gather expected successors with readable actions."
  (loop for (action deck-index cycle) in expected-data
        for old-exist-flags = (mask-field (byte 52 0) starting-state)
        for exist-flags = (if (listp action)
                              (apply #'remove-cards deck old-exist-flags action)
                            old-exist-flags)
        collect (cons action (make-state exist-flags deck-index cycle))))

(defun actual-successors (starting-state)
  "Gather actual successors with readable actions."
  (loop for state in (ps::state-successors starting-state)
        collect (cons (ps::action starting-state state) state)))

(defun successors-diffs (deck starting-state expected-data)
  "Compare expected and actual successors and test what's missing in each."
  (ps::with-deck deck
    (let ((expected (expected-successors deck starting-state expected-data))
          (actual (actual-successors starting-state)))
      ;; check what's in expected but not in actual
      (is (null (set-difference expected actual :test #'equal)))
      ;; check what's in actual but not in expected
      (is (null (set-difference actual expected :test #'equal))))))

(test state-successors-part1
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
       
(test state-successors-cycle3
  (successors-diffs *deck* (make-state (1- (expt 2 52)) 52 3)
                    '( ; case 5: can't recycle because it's cycle 3
                      (("Jd" "2h") 52 3) ; case 9
                      (("Qd" "Ah") 52 3) ; case 9
                      (("Kd") 52 3) ; case 6
                      (("Ks") 52 3)))) ; case 8

(test state-successors-recycle
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

(test solve-minimal
  (is (= 15 (length (solve *shortest-solution-deck*)))))
