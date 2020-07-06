(defpackage #:pyramid-solver
  (:nicknames #:ps)
  (:use #:common-lisp))

(in-package #:pyramid-solver)

;;; Card and Deck definitions

(deftype card-rank ()
  "Card ranks are one of the following characters: A 2 3 4 5 6 7 8 9 T J Q K.
   Ranks are uppercase (when applicable) and 10 is a T to make it one letter."
  '(member #\A #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\T #\J #\Q #\K))

(deftype card-suit ()
  "Card suits are one of the following characters: c d h s.
   Suits are always lowercase."
  '(member #\c #\d #\h #\s))

(defun cardp (object)
  "Return true if the object is a card.
   Cards are two-letter strings made of a rank followed by a suit."
  (and (stringp object)
       (= (length object) 2)
       (typep (char object 0) 'card-rank)
       (typep (char object 1) 'card-suit)))

(defvar *all-cards*
  '("Ac" "2c" "3c" "4c" "5c" "6c" "7c" "8c" "9c" "Tc" "Jc" "Qc" "Kc"
    "Ad" "2d" "3d" "4d" "5d" "6d" "7d" "8d" "9d" "Td" "Jd" "Qd" "Kd"
    "Ah" "2h" "3h" "4h" "5h" "6h" "7h" "8h" "9h" "Th" "Jh" "Qh" "Kh"
    "As" "2s" "3s" "4s" "5s" "6s" "7s" "8s" "9s" "Ts" "Js" "Qs" "Ks")
  "A list of all cards in a standard 52-card deck. The cards in this list are
   present in a standardized order used throughout the program.")

(defun card-value (card)
  "Return the card's value according to Pyramid Solitaire rules.
   Aces are always 1, Jacks are 11, Queens are 12, and Kings are 13."
  (1+ (position (char card 0) "A23456789TJQK")))

(defun count-cards (list)
  "Return a hash table showing how many times each card appears in the list.
   If the card is never in the list, it'll still be in the hash table but with
   a count of zero. Objects that aren't cards are ignored."
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (card *all-cards*)
      (setf (gethash card counts) 0))
    (dolist (object list counts)
      (when (cardp object)
        (incf (gethash object counts))))))

(defun missing-cards (list)
  "Return a list of the cards in a standard deck that aren't in the given list.
   The missing cards are returned in a consistent order, otherwise you could
   just use set-difference."
  (loop with counts = (count-cards list)
        for card in *all-cards*
        when (zerop (gethash card counts))
        collect card))

(defun duplicate-cards (list)
  "Return a list of the cards in the list that appear multiple times.
   For the caller's benefit, cards are returned in the order they appear, and
   if a card is duplicated N times, it'll be in the result list N times."
  (loop with counts = (count-cards list)
        for object in list
        when (and (cardp object)
                  (> (gethash object counts) 1))
        collect object))

(defun deckp (object)
  "Return true if the object is a standard 52-card deck.
   Decks are lists containing every card exactly once, in any order."
  (and (listp object)
       (handler-case (= (length object) 52)
         (type-error () nil)) ; handle improper lists
       (null (missing-cards object))))

(defun string-to-card-list (string)
  "Return a list of all cards in the string.
   The result may or may not be a standard deck."
  (loop for i from 0 below (1- (length string))
        when (and (typep (char string i) 'card-rank)
                  (typep (char string (1+ i)) 'card-suit))
        collect (subseq string i (+ i 2))))
