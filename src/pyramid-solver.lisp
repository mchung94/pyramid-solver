;;;; This program finds the shortest possible solution to Pyramid Solitaire
;;;; games under Microsft Solitaire Collection's rules, or lets you know that
;;;; there is no solution.
;;;;
;;;; It's easy to write a breadth-first search to solve Pyramid Solitaire, but
;;;; without a lot of optimization work it could take hours to run and/or use
;;;; too much memory. This program does two main things to run fast and small:
;;;; 1. Represent the state of the game using 43 bits, a fixnum in 64-bit
;;;;    Common Lisp implementations. Still, it might need a few gigabytes of
;;;;    memory in the worst case (e.g. exhaustively searching only to find out
;;;;    there's no solution).
;;;; 2. Precalculate almost everything we need to run the search, so that the
;;;;    search loop spends the majority of its time on hashtable operations.
;;;;    The precalculation step needs to run fast so that code might look ugly.
;;;;
;;;; Version 1 of this program focused on optimizing the average-case time.
;;;; My current opinion is that the worst-case time and space requirements are
;;;; the most important priority after correctness.
;;;; This version is an investigation into improving the worst case speed and
;;;; memory usage at the cost of making the average case worse.

(defpackage #:pyramid-solver
  (:use #:common-lisp))

(in-package #:pyramid-solver)

;;; Card and Deck Definitions
;;; For simplicity, cards are just strings containing rank and suit.
;;; These functions don't need to be fast because the speed comes from
;;; precalculating everything we need for the solution search later on.

(deftype card-rank ()
  "Card ranks are the characters A 2 3 4 5 6 7 8 9 T J Q K for
   Ace/2/3/4/5/6/7/8/9/10/Jack/Queen/King."
  '(member #\A #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\T #\J #\Q #\K))

(deftype card-suit ()
  "Card suits are the characters c d h s for Clubs/Diamonds/Hearts/Spades."
  '(member #\c #\d #\h #\s))

(defvar *cards*
  '("Ac" "2c" "3c" "4c" "5c" "6c" "7c" "8c" "9c" "Tc" "Jc" "Qc" "Kc"
    "Ad" "2d" "3d" "4d" "5d" "6d" "7d" "8d" "9d" "Td" "Jd" "Qd" "Kd"
    "Ah" "2h" "3h" "4h" "5h" "6h" "7h" "8h" "9h" "Th" "Jh" "Qh" "Kh"
    "As" "2s" "3s" "4s" "5s" "6s" "7s" "8s" "9s" "Ts" "Js" "Qs" "Ks")
  "A list of all cards in a standard 52-card deck.")

(defun card-value (card)
  "Return the numeric value of a card according to Pyramid Solitaire rules.
   Aces are always 1, Jacks are 11, Queens are 12, and Kings are 13."
  (1+ (position (char card 0) "A23456789TJQK")))

(defun cards-in-string (string)
  "Return a list of the cards in the string."
  (loop for i from 0 below (1- (length string))
        when (and (typep (char string i) 'card-rank)
                  (typep (char string (1+ i)) 'card-suit))
        collect (subseq string i (+ i 2))))

(defun deckp (object)
  "Return true if the object is a standard 52-card deck. Decks of cards are
   lists without any cards missing, duplicated, or objects that aren't cards."
  (and (listp object)
       (handler-case (= 52 (length object))
         (type-error () nil))
       (null (set-difference *cards* object :test #'equal))))

;;; Definitions related to the 28-card Pyramid structure
;;; This contains precalculated data that is used to keep data strutures small.

(deftype pyramid-flags ()
  "Pyramid flags are integers used as bit fields to indicate which cards remain
   on the table. They are indexed this way:
                0
              1   2
            3   4   5
          6   7   8   9
       10  11  12  13  14
     15  16  17  18  19  20
   21  22  23  24  25  26  27"
  '(unsigned-byte 28))

(deftype pyramid-index ()
  "An index into the 28 pyramid cards."
  '(integer 0 27))

(deftype pyramid-id ()
  "An integer uniquely identifying one of the 1430 valid pyramid-flags values.
   Although there are 268435456 possible 28-bit values, only 1430 are valid
   pyramid-flags values because a card can only be removed from the pyramid when
   there are no cards blocking it from below. For example, the card at index 12
   can only be removed when the cards at indexes 17, 18, 23, 24, and 25 are
   removed first."
  '(integer 0 1429))

(defun calculate-pyramid-flags ()
  "Return a vector of all 1430 valid pyramid-flags values in sorted order."
  ;; This is designed to be fast so it won't slow down interactive development.
  ;; It builds lists of integer bit fields of each pyramid row, bottom up.
  ;; One example is '(#b1 #b11 #b111 #b1111 #b11111 #b110111 #b1100100), which
  ;; will be converted to #b1100100110111111111111111111 by make-pyramid-flags.
  ;; This isn't the fastest way, but it's much faster than iterating through
  ;; all possible 28-bit values and checking if it's a valid pyramid-flags.
  (labels ((make-pyramid-flags (pyramid)
             (reduce #'logior (mapcar #'ash pyramid '(0 1 3 6 10 15 21))))
           (card-exists-p (index row)
             (logbitp index row))
           (card-uncovered-p (index next-row)
             (zerop (mask-field (byte 2 index) next-row)))
           (valid-row-p (row row-size next-row)
             (loop for i from 0 below row-size
                   always (or (card-exists-p i row) (card-uncovered-p i next-row))))
           (add-row-to-pyramids (row-size pyramids)
             (loop for row from 0 below (ash 1 row-size)
                   nconc (loop for pyramid in pyramids
                               when (valid-row-p row row-size (first pyramid))
                               collect (cons row pyramid))))
           (build-pyramid-lists (row-size &optional pyramids)
             (if (= 1 row-size)
                 (add-row-to-pyramids 1 pyramids)
               (build-pyramid-lists (1- row-size) (add-row-to-pyramids row-size pyramids)))))
    (let* ((bottom-rows (loop for row from 0 below (ash 1 7) collect (list row)))
           (pyramid-lists (build-pyramid-lists 6 bottom-rows)))
      (sort (map 'vector #'make-pyramid-flags pyramid-lists) #'<))))

(defun calculate-pyramid-flags-to-id-mapping (all-pyramid-flags)
  "Return a mapping from a pyramid-flags value to its pyramid-id."
  (loop with mapping = (make-hash-table)
        for pyramid-id from 0
        for pyramid-flags across all-pyramid-flags
        do (setf (gethash pyramid-flags mapping) pyramid-id)
        finally (return mapping)))

(defun calculate-pyramid-indexes (pyramid-flags)
  "Return a list of pyramid-indexes of cards that are in the pyramid."
  (loop for pyramid-index from 0 below 28
        when (logbitp pyramid-index pyramid-flags)
        collect pyramid-index))

;;; The following special variables hold precalculated data that never changes
;;; and must not be modified.
(defvar *pyramid-flags* (calculate-pyramid-flags)
  "A vector of all 1430 pyramid-flags values, sorted. The index into the vector
   is the pyramid-id for the pyramid-flags value.")

(defvar *pyramid-id* (calculate-pyramid-flags-to-id-mapping *pyramid-flags*)
  "A hash table mapping pyramid-flags to pyramid-id.")

(defvar *pyramid-indexes* (map 'vector #'calculate-pyramid-indexes *pyramid-flags*)
  "A vector mapping pyramid-id to a list of pyramid-indexes of existing cards.")
