(defpackage #:pyramid-solver
  (:nicknames #:ps)
  (:use #:common-lisp))

(in-package #:pyramid-solver)

;;; Card and Deck definitions
(deftype rank ()
  "Card ranks are characters: A23456789TJQK for Ace/2/3/4/5/6/7/8/9/Ten/Jack/Queen/King.
   They are uppercase (when applicable) and 10 is T so they're all single characters."
  '(member #\A #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\T #\J #\Q #\K))

(deftype suit ()
  "Card suits are lowercase characters: cdhs for Clubs/Diamonds/Hearts/Spades."
  '(member #\c #\d #\h #\s))

(defun cardp (object)
  "Return true if the object is a card.
   Cards are two-letter strings made of a rank followed by a suit."
  (and (stringp object)
       (= 2 (length object))
       (typep (char object 0) 'rank)
       (typep (char object 1) 'suit)))

(defvar *cards*
  '("Ac" "2c" "3c" "4c" "5c" "6c" "7c" "8c" "9c" "Tc" "Jc" "Qc" "Kc"
    "Ad" "2d" "3d" "4d" "5d" "6d" "7d" "8d" "9d" "Td" "Jd" "Qd" "Kd"
    "Ah" "2h" "3h" "4h" "5h" "6h" "7h" "8h" "9h" "Th" "Jh" "Qh" "Kh"
    "As" "2s" "3s" "4s" "5s" "6s" "7s" "8s" "9s" "Ts" "Js" "Qs" "Ks")
  "A list of all cards in a standard 52-card deck.")

(defun card-value (card)
  "Return the card's numeric value according to Pyramid Solitaire rules.
   Aces are always equal to 1 and Jacks=11, Queens=12, and Kings=13."
  (1+ (position (char card 0) "A23456789TJQK")))

(defun missing-cards (list)
  "Return a list of the cards of a standard 52-card deck that aren't in the given list."
  ;; return the cards in order, don't use set-difference
  (loop for card in *cards*
        unless (member card list :test #'equal)
        collect card))

(defun duplicate-cards (list)
  "Return a list of the cards in the list that appear multiple times."
  ;; if the card appears N times, put it in the result N times so the caller
  ;; has an idea of how many times it appears
  (let ((counter (make-hash-table :test #'equal)))
    (dolist (object list)
      (setf (gethash object counter) (1+ (or (gethash object counter) 0))))
    (loop for object in list
          when (and (cardp object) (> (gethash object counter) 1))
          collect object)))

(defun deckp (object)
  "A deck is a list of all cards in a standard 52-card deck, in any order.
   It can't have any missing cards, duplicate cards, or things that aren't cards in it."
  (and (listp object)
       (handler-case (= 52 (length object))
         (type-error () nil)) ; handle improper lists
       (null (missing-cards object))))

(defun string-to-card-list (string)
  "Return a list of the cards in the string. The result may or may not be a deck."
  (loop for i from 0 below (1- (length string))
        when (and (typep (char string i) 'rank)
                  (typep (char string (1+ i)) 'suit))
        collect (subseq string i (+ i 2))))
