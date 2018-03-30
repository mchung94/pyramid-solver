(defpackage #:pyramid-solver-tests
  (:use #:cl #:fiveam))
(in-package #:pyramid-solver-tests)

(def-suite pyramid-solver-tests)
(in-suite pyramid-solver-tests)

(defparameter *ordered-deck*
  '("Ac" "2c" "3c" "4c" "5c" "6c" "7c" "8c" "9c" "Tc" "Jc" "Qc" "Kc"
    "Ad" "2d" "3d" "4d" "5d" "6d" "7d" "8d" "9d" "Td" "Jd" "Qd" "Kd"
    "Ah" "2h" "3h" "4h" "5h" "6h" "7h" "8h" "9h" "Th" "Jh" "Qh" "Kh"
    "As" "2s" "3s" "4s" "5s" "6s" "7s" "8s" "9s" "Ts" "Js" "Qs" "Ks"))

(test missing-cards
  (is (null (ps:missing-cards *ordered-deck*)))
  (is (equal '("Ac") (ps:missing-cards (rest *ordered-deck*)))))

(test duplicate-cards
  (is (null (ps:duplicate-cards *ordered-deck*)))
  (is (equal '("Ks" "Ks") (ps:duplicate-cards (cons "Ks" *ordered-deck*))))
  (is (null (ps:duplicate-cards (cons 123 *ordered-deck*)))))

(test malformed-cards
  (is (null (ps:malformed-cards *ordered-deck*)))
  (is (equal '("d3") (ps:malformed-cards (cons "d3" *ordered-deck*)))))

(test standard-deck-p
  (is (ps:standard-deck-p *ordered-deck*))
  (is (not (ps:standard-deck-p (rest *ordered-deck*))))
  (is (not (ps:standard-deck-p (cons "Ks" *ordered-deck*))))
  (is (not (ps:standard-deck-p (cons "d3" *ordered-deck*))))
  (is (not (ps:standard-deck-p (coerce *ordered-deck* 'vector))))
  (let ((dotted-list (copy-list *ordered-deck*)))
    (setf (cdr (last dotted-list 2)) "Ks")
    (is (not (ps:standard-deck-p dotted-list)))))

(test string->card-list
  (is (equal *ordered-deck* (ps:string->card-list "
            Ac M
          2c  3cC
        4c  5c  6ca
      7c  8c  9c  Tc0
    Jc  Qc  Kc  Ad  102d
  3d  4d  5d  6d  7d  ]8d
9d  Td  Jd  Qd  Kd  Ah  @2h
3h 4h 5h 6h 7h 8h 9h #Th JhQh Kh As 2s 3s 4s 5s 6s 7s 8s 9s Ts Js Qs Ks"))))



;;; Tests on the precalculated PYRAMID-FLAGs
;;; Diagram of Pyramid Indexes for reference
;;;             00
;;;           01  02
;;;         03  04  05
;;;       06  07  08  09
;;;     10  11  12  13  14
;;;   15  16  17  18  19  20
;;; 21  22  23  24  25  26  27

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
  "Return the row and column of *PYRAMID-INDEXES* that contains PYRAMID-INDEX."
  (dotimes (row 7)
    (dotimes (column (- 7 row))
      (when (= (aref *pyramid-indexes* row column) pyramid-index)
        (return-from locate-pyramid-index (list row column))))))

(defun ancestors (pyramid-index)
  "Return a list of all ancestor pyramid indexes of PYRAMID-INDEX."
  (sort (loop with location = (locate-pyramid-index pyramid-index)
              for row from 0 to (first location)
              nconc (loop for col from 0 to (second location)
                          for index = (aref *pyramid-indexes* row col)
                          when (/= index pyramid-index)
                          collect index))
        #'<))

(defun descendants (pyramid-index)
  "Return a list of all descendant pyramid indexes of PYRAMID-INDEX."
  (sort (loop with location = (locate-pyramid-index pyramid-index)
              for row from (first location) below 7
              nconc (loop for col from (second location) below (- 7 row)
                          for index = (aref *pyramid-indexes* row col)
                          when (/= index pyramid-index)
                          collect index))
        #'<))

(defun valid-pyramid-flags-p (pyramid-flags)
  "Return T if PYRAMID-FLAGS has no covered cards removed."
  (flet ((card-exists-p (pyramid-index)
           (logbitp pyramid-index pyramid-flags)))
    (dotimes (pyramid-index 28 t)
      (when (and (not (card-exists-p pyramid-index))
                 (some #'card-exists-p (descendants pyramid-index)))
        (return-from valid-pyramid-flags-p nil)))))

(test all-pyramid-flags
  (let ((all-pyramid-flags (ps::all-pyramid-flags)))
    (is (= 1430 (length all-pyramid-flags)))
    (loop for i from 0
          for pyramid-flags across all-pyramid-flags
          do (is (valid-pyramid-flags-p pyramid-flags)
                 "Index: ~A, ~28,'0B is not a valid PYRAMID-FLAGS"
                 i pyramid-flags))
    (is (= #b0000000000000000000000000000 (svref all-pyramid-flags 0)))
    (is (= #b1111111111111111111111111111 (svref all-pyramid-flags 1429)))
    (is (equalp ps::*pyramid-flags* all-pyramid-flags))))

(test pyramid-flags->id
  (is (= 1430 (hash-table-count ps::*pyramid-flags->id*)))
  (loop for pyramid-id from 0
        for pyramid-flags across ps::*pyramid-flags*
        do (is (= pyramid-id (ps::pyramid-flags->id pyramid-flags)))))

(test pyramid-existing-indexes
  (let ((pyramid-existing-indexes (map 'vector
                                       #'ps::pyramid-existing-indexes
                                       ps::*pyramid-flags*)))
    (is (= 1430 (length pyramid-existing-indexes)))
    (flet ((mask (index)
             (ash 1 index)))
      (loop for i from 0
            for expected across ps::*pyramid-flags*
            for existing-indexes across pyramid-existing-indexes
            for actual = (reduce #'logior (mapcar #'mask existing-indexes))
            do (is (= expected actual)
                   "Index: ~A, Expected: ~A, Actual: ~A"
                   i expected actual)))
    (is (null (svref pyramid-existing-indexes 0)))
    (is (equal (loop for i from 0 to 27 collect i)
               (svref pyramid-existing-indexes 1429)))
    (is (equalp ps::*pyramid-existing-indexes* pyramid-existing-indexes))))

(defun pyramid-uncovered-indexes (pyramid-flags)
  "Return a list of the uncovered pyramid cards in PYRAMID-FLAGS."
  (labels ((card-exists-p (pyramid-index)
             (logbitp pyramid-index pyramid-flags))
           (uncoveredp (pyramid-index)
             (and (card-exists-p pyramid-index)
                  (notany #'card-exists-p (descendants pyramid-index)))))
    (remove-if (complement #'uncoveredp) (loop for i from 0 to 27 collect i))))

(test pyramid-uncovered-indexes
  (let ((pyramid-uncovered-indexes (map 'vector
                                        #'ps::pyramid-uncovered-indexes
                                        ps::*pyramid-flags*)))
    (is (= 1430 (length pyramid-uncovered-indexes)))
    (loop for i from 0
          for pyramid-flags across ps::*pyramid-flags*
          for expected = (pyramid-uncovered-indexes pyramid-flags)
          for actual across pyramid-uncovered-indexes
          do (is (equal expected actual)
                 "Index: ~A, Expected: ~A, Actual: ~A"
                 i expected actual))
    (is (null (svref pyramid-uncovered-indexes 0)))
    (is (equal '(21 22 23 24 25 26 27)
               (svref pyramid-uncovered-indexes 1429)))
    (is (equalp ps::*pyramid-uncovered-indexes* pyramid-uncovered-indexes))))



;;; Tests on deck precalculations
(defun card-value (card)
  "Return the numeric value of a card (1 - 13)."
  (ecase (char card 0)
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

(defun card-values (deck)
  "Return a vector containing the numeric value of each card in the deck."
  (map 'vector #'card-value deck))

(test card-values
  (is (equalp (card-values *ordered-deck*)
              (ps::card-values *ordered-deck*))))

(defun state->deck-flags (state)
  "Convert STATE into a 52-bit DECK-FLAGs value that shows which cards exist."
  (logior (svref ps::*pyramid-flags* (mask-field (byte 11 0) state))
          (mask-field (byte 24 28) state)))

(defun set-bit-indexes (deck-flags)
  "Return a list of which bits are set to 1 in 52-bit DECK-FLAGS."
  (loop for i from 0 below 52
        when (logbitp i deck-flags)
        collect i))

(defun removed-cards (old-state new-state)
  "Return a list of card indexes removed between the two states."
  (set-bit-indexes (logxor (state->deck-flags old-state)
                           (state->deck-flags new-state))))

(defun valid-successor-masks-p (pyramid-id stock-index waste-index cycle card-values masks)
  "Return T if MASKS are the correct successor masks for the given state data."
  (let ((uncovered-indexes (svref ps::*pyramid-uncovered-indexes* pyramid-id))
        (pyramid-flags (svref ps::*pyramid-flags* pyramid-id))
        (expected ()))
    (labels ((mask1p (i)
               "Create a mask to remove one pyramid card from a state."
               (logxor pyramid-id (ps::pyramid-flags->id (logxor pyramid-flags (ash 1 i)))))
             (mask2p (i1 i2)
               "Create a mask to remove two pyramid cards from a state."
               (logxor pyramid-id (ps::pyramid-flags->id
                                   (logxor pyramid-flags (logior (ash 1 i1) (ash 1 i2))))))
             (mask1sw (i)
               "Create a mask to remove one stock/waste card from a state."
               (ash 1 i))
             (mask2sw (i1 i2)
               "Create a mask to remove two stock/waste cards from a state."
               (logior (ash 1 i1) (ash 1 i2)))
             (mask1sw1p (swi pyi)
               "Create a mask to remove one stock/waste card and one pyramid card from a state."
               (logior (mask1sw swi) (mask1p pyi)))
             (maskdraw ()
               "Create a mask to draw a card from the stock pile."
               (ash (logxor stock-index (1+ stock-index)) 13))
             (maskrecycle ()
               "Create a mask to recycle the waste pile."
               (logior (ash (logxor 52 28) 13) (ash (logxor cycle (1+ cycle)) 11))))
      ;; remove top card of the stock pile if it's a king
      (when (and (/= stock-index 52) (= 13 (svref card-values stock-index)))
        (push (mask1sw stock-index) expected))
      ;; remove top card of the waste pile if it's a king
      (when (and (/= waste-index 27) (= 13 (svref card-values waste-index)))
        (push (mask1sw waste-index) expected))
      ;; remove top stock/waste pile cards if they add up to 13
      (when (and (/= stock-index 52)
                 (/= waste-index 27)
                 (= 13 (+ (svref card-values stock-index) (svref card-values waste-index))))
        (push (mask2sw stock-index waste-index) expected))
      (dolist (index uncovered-indexes)
        ;; remove uncovered kings on the pyramid
        (when (= 13 (svref card-values index))
          (push (mask1p index) expected))
        ;; remove one stock card with the uncovered pyramid card if they add up to 13
        (when (and (/= stock-index 52)
                   (= 13 (+ (svref card-values index) (svref card-values stock-index))))
          (push (mask1sw1p stock-index index) expected))
        ;; remove one waste card with the uncovered pyramid card if they add up to 13
        (when (and (/= waste-index 27)
                   (= 13 (+ (svref card-values index) (svref card-values waste-index))))
          (push (mask1sw1p waste-index index) expected)))
      ;; remove uncovered pyramid cards that add up to 13
      (loop for (i . others) on uncovered-indexes
            do (dolist (j others)
                 (when (= 13 (+ (svref card-values i) (svref card-values j)))
                   (push (mask2p i j) expected))))
      ;; draw a card if possible
      (when (/= stock-index 52)
        (push (maskdraw) expected))
      ;; recycle the waste pile if possible
      (when (and (< cycle 2) (= stock-index 52))
        (push (maskrecycle) expected))
      (not (or (set-difference expected masks) (set-difference masks expected))))))

(defun test-successor-masks (card-values)
  "Check all possible successor masks for the given card values, fail if anything is wrong."
  (let ((successor-masks (ps::successor-masks card-values)))
    (flet ((smref (p s w c)
             (svref (svref (svref (svref successor-masks p) s) w) c)))
      (dotimes (pyramid-id 1430)
        (dotimes (stock-index 53)
          (when (<= 28 stock-index 52)
            (dotimes (waste-index 52)
              (when (< 26 waste-index stock-index)
                (dotimes (cycle 3)
                  (unless (valid-successor-masks-p
                           pyramid-id stock-index waste-index cycle card-values
                           (smref pyramid-id stock-index waste-index cycle))
                    (fail "Bad masks - p: ~D s: ~D w: ~D c: ~D"
                          pyramid-id stock-index waste-index cycle)))))))))))

(test successor-masks
  (test-successor-masks (card-values *ordered-deck*)))

(defun h-cost (pyramid-id card-values)
  "Estimate the number of steps to clear the pyramid cards for the PYRAMID-ID and CARD-VALUES."
  (flet ((num-cards (value)
           (count value
                  (svref ps::*pyramid-existing-indexes* pyramid-id)
                  :key (lambda (index) (svref card-values index)))))
    (+ (num-cards 13)
       (max (num-cards 1) (num-cards 12))
       (max (num-cards 2) (num-cards 11))
       (max (num-cards 3) (num-cards 10))
       (max (num-cards 4) (num-cards 9))
       (max (num-cards 5) (num-cards 8))
       (max (num-cards 6) (num-cards 7)))))

(test h-cost
  (let* ((card-values (card-values *ordered-deck*))
         (h-costs (ps::h-costs card-values)))
    (dotimes (pyramid-id 1430)
      (is (= (h-cost pyramid-id card-values) (svref h-costs pyramid-id))))))

(defun pyramid-match-p (pyramid-index existing-indexes card-values)
  "Return T if the card at PYRAMID-INDEX can be removed by another pyramid card."
  (flet ((unrelated-existing-indexes ()
           "Return EXISTING-INDEXES that aren't covering or covered by PYRAMID-INDEX."
           (loop for index in existing-indexes
                 unless (or (find index (descendants pyramid-index))
                            (find index (ancestors pyramid-index)))
                 collect index)))
    (loop with matching-value = (- 13 (svref card-values pyramid-index))
          for index in (unrelated-existing-indexes)
          thereis (= matching-value (svref card-values index)))))

(defun unclearable-mask (value card-values)
  "Return a mask where the bits for the stock/waste cards that are (- 13 VALUE) are set to 1."
  (loop with mask = 0
        with matching-value = (- 13 value)
        for i from 28 to 51
        when (= matching-value (svref card-values i))
        do (setf mask (logior mask (ash 1 i)))
        finally (return mask)))
  
(defun unclearable-masks (pyramid-id card-values)
  (let ((masks (loop with existing-indexes = (svref ps::*pyramid-existing-indexes* pyramid-id)
                     for pyramid-index in existing-indexes
                     unless (or (= 13 (svref card-values pyramid-index))
                                (pyramid-match-p pyramid-index existing-indexes card-values))
                     collect (unclearable-mask (svref card-values pyramid-index) card-values))))
    (if (some #'zerop masks)
        '(0)
      masks)))
    
(test unclearable-masks
  (let* ((card-values (card-values *ordered-deck*))
         (masks (ps::unclearable-masks card-values)))
    (dotimes (pyramid-id 1430)
      (is (equal (unclearable-masks pyramid-id card-values) (svref masks pyramid-id))))))

;;; To perform full testing of successors we should have cases that:
;;; 1) Draw a card because there's cards on in the stock pile.
;;; 2) Don't draw a card because the stock pile is empty.
;;; 3) Recycle the waste pile because the stock pile is empty.
;;; 4) Don't recycle because the stock pile isn't empty.
;;; 5) Don't recycle because we already recycled twice.
;;; 6) Remove a King on the pyramid.
;;; 7) Remove a King from the top of the stock pile.
;;; 8) Remove a King from the top of the waste pile.
;;; 9) Remove a pair of cards, both on the pyramid.
;;; 10) Remove a pair of cards, one on the pyramid and one on the stock pile.
;;; 11) Remove a pair of cards, one on the pyramid and one on the waste pile.
;;; 12) Remove a pair of cards, one on the stock pile and one on the waste pile.
;;;
;;; *ordered-deck* as an example to look at for the tests below:
;;;             Ac
;;;           2c  3c
;;;         4c  5c  6c
;;;       7c  8c  9c  Tc
;;;     Jc  Qc  Kc  Ad  2d
;;;   3d  4d  5d  6d  7d  8d
;;; 9d  Td  Jd  Qd  Kd  Ah  2h
;;; 3h 4h 5h 6h 7h 8h 9h Th Jh Qh Kh As 2s 3s 4s 5s 6s 7s 8s 9s Ts Js Qs Ks

(defun make-state (deck cards-to-remove stock-index cycle)
  "Return a state based on DECK minus CARDS-TO-REMOVE, plus the given STOCK-INDEX and CYCLE."
  (let ((deck-flags #xfffffffffffff))
    (dolist (card cards-to-remove)
      (setf deck-flags (logxor deck-flags (ash 1 (position card deck :test #'string=)))))
    (logior (mask-field (byte 24 28) deck-flags)
            (ash stock-index 13)
            (ash cycle 11)
            (ps::pyramid-flags->id (mask-field (byte 28 0) deck-flags)))))

(defun check-successors (deck state &rest expected-successors)
  "Compare expected and actual successors and test what's missing in each."
  (let* ((card-values (card-values deck))
         (successor-masks (ps::successor-masks card-values))
         (pyramid-id (ps::state-pyramid-id state))
         (actual (ps::state-successors state pyramid-id successor-masks)))
    (is (null (set-difference expected-successors actual :test #'equal)))
    (is (null (set-difference actual expected-successors :test #'equal)))))

(test state-successors-draw
  (check-successors *ordered-deck* (make-state *ordered-deck* () 28 0)
                    ; case 4: can't recycle because the stock isn't empty
                    (make-state *ordered-deck* () 29 0) ; case 1
                    (make-state *ordered-deck* '("Jd" "2h") 28 0) ; case 9
                    (make-state *ordered-deck* '("Qd" "Ah") 28 0) ; case 9
                    (make-state *ordered-deck* '("Kd") 28 0) ; case 6
                    (make-state *ordered-deck* '("Td" "3h") 29 0))) ; case 10

(test state-successors-recycle
  (check-successors *ordered-deck* (make-state *ordered-deck* () 52 0)
                    ; case 2: can't draw because the stock is empty
                    (make-state *ordered-deck* () 28 1) ; case 3
                    (make-state *ordered-deck* '("Jd" "2h") 52 0) ; case 9
                    (make-state *ordered-deck* '("Qd" "Ah") 52 0) ; case 9
                    (make-state *ordered-deck* '("Kd") 52 0) ; case 6
                    (make-state *ordered-deck* '("Ks") 52 0))) ; case 8

(test state-successors-end-of-last-cycle
  (check-successors *ordered-deck* (make-state *ordered-deck* () 52 2)
                    ; case 5: can't recycle when stock is empty because we already recycled twice
                    (make-state *ordered-deck* '("Jd" "2h") 52 2) ; case 9
                    (make-state *ordered-deck* '("Qd" "Ah") 52 2) ; case 9
                    (make-state *ordered-deck* '("Kd") 52 2) ; case 6
                    (make-state *ordered-deck* '("Ks") 52 2))) ; case 8

(test state-successors-remove-king-from-stock
  (check-successors *ordered-deck* (make-state *ordered-deck* () 51 0)
                    (make-state *ordered-deck* () 52 0) ; case 1
                    (make-state *ordered-deck* '("Jd" "2h") 51 0) ; case 9
                    (make-state *ordered-deck* '("Qd" "Ah") 51 0) ; case 9
                    (make-state *ordered-deck* '("Kd") 51 0) ; case 6
                    (make-state *ordered-deck* '("Ah" "Qs") 51 0) ; case 11
                    (make-state *ordered-deck* '("Ks") 52 0))) ; case 7

(test state-successors-stock-and-waste-pair
  (check-successors *ordered-deck* (make-state *ordered-deck* () 32 0)
                    (make-state *ordered-deck* '("6h" "7h") 33 0) ; case 12
                    (make-state *ordered-deck* '() 33 0) ; case 1
                    (make-state *ordered-deck* '("Jd" "2h") 32 0) ; case 9
                    (make-state *ordered-deck* '("Qd" "Ah") 32 0) ; case 9
                    (make-state *ordered-deck* '("Kd") 32 0))) ; case 6

(test new-fringe-is-empty
  (let ((fringe (ps::create-bucket-queue 102)))
    (is-true (ps::bucket-queue-empty-p fringe))))

(test fringe-add-remove-empty
  (let ((fringe (ps::create-bucket-queue 102)))
    (ps::bucket-queue-add fringe "one" 1)
    (is (string= "one" (ps::bucket-queue-remove fringe)))
    (is-true (ps::bucket-queue-empty-p fringe))))

(test fringe-remove-when-empty-returns-nil
  (let ((fringe (ps::create-bucket-queue 102)))
    (is (not (ps::bucket-queue-remove fringe)))))

(test fringe-add-remove-updates-front-correctly
  (let ((fringe (ps::create-bucket-queue 102)))
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
  (let* ((deck (ps::string->card-list "
            Kd
          Kc  Qh
        Ah  7d  6d
      8d  5d  9d  4d
    Td  3d  Jd  2d  Qd
  Ad  7c  6c  8c  5c  9c
4c  Tc  3c  Jc  2c  Qc  Ac
6h 7h 5h 8h 4h 9h 3h Th 2h Jh Kh As 2s 3s 4s 5s 6s 7s 8s 9s Ts Js Qs Ks"))
         (solution (ps:solve deck)))
    (is (= 15 (length solution)))))

(test solve-44-step-solution
  (let* ((deck (ps::string->card-list "
            Th
          Js  Jh
        9c  Qd  5c
      2d  9h  Td  4h
    Qs  9d  3s  8d  Kh
  6c  3h  6d  8c  Kc  Ah
Qh  Tc  9s  Kd  8s  4s  2c
4c Jc 7c Jd 8h 6s 5d 3c 4d 3d 6h Ts 5s Ks 7d Ac 7s 2s Qc 2h 5h As 7h Ad"))
         (solution (ps:solve deck)))
    (is (= 44 (length solution)))))
