(defpackage #:pyramid-solver/tests
  (:use #:common-lisp #:1am))

(in-package #:pyramid-solver/tests)

(defparameter *ordered-deck*
  (loop for suit across "cdhs"
        nconc (loop for rank across "A23456789TJQK"
                    collect (format nil "~C~C" rank suit))))

(test cardp ()
  (dolist (card *ordered-deck*)
    (is (ps::cardp card))))

(test card-value ()
  (flet ((card-value (card)
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
             (#\K 13))))
    (dolist (card *ordered-deck*)
      (is (= (card-value card) (ps::card-value card))))))

(test missing-cards ()
  (is (null (ps::missing-cards *ordered-deck*)))
  (is (equal '("Qs" "Ks") (ps::missing-cards (butlast *ordered-deck* 2))))
  (is (equal '("Ac" "2c") (ps::missing-cards (cddr *ordered-deck*)))))

(test duplicate-cards ()
  (is (null (ps::duplicate-cards *ordered-deck*)))
  (is (equal '("Ks" "Ks") (ps::duplicate-cards (cons "Ks" *ordered-deck*)))))

(test deckp ()
  (is (ps::deckp *ordered-deck*))
  (is (not (ps::deckp (rest *ordered-deck*))))
  (is (not (ps::deckp (cons "Ks" *ordered-deck*))))
  (is (not (ps::deckp (cons "ks" *ordered-deck*))))
  (is (not (ps::deckp (coerce *ordered-deck* 'vector))))
  (let ((dotted-list (copy-list *ordered-deck*)))
    (setf (cdr (last dotted-list 2)) "Ks")
    (is (not (ps::deckp dotted-list)))))

(test string-to-card-list
  (is (equal *ordered-deck* (ps::string-to-card-list "
              Ac M
            2c  3cC
          4c  5c  6ca
        7c  8c  9c  Tc0
      Jc  Qc  Kc  Ad  102d
    3d  4d  5d  6d  7d  ]8d
  9d  Td  Jd  Qd  Kd  Ah  @2h
  3h 4h 5h 6h 7h 8h 9h #Th JhQh Kh As 2s 3s 4s 5s 6s 7s 8s 9s Ts Js Qs Ks"))))
