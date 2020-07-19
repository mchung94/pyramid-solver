(defpackage #:pyramid-solver/tests
  (:use #:common-lisp #:1am))

(in-package #:pyramid-solver/tests)

(defvar *ordered-deck* (loop for suit across "cdhs"
                             nconc (loop for rank across "A23456789TJQK"
                                         collect (format nil "~C~C" rank suit))))

(test card-value ()
  (flet ((value (card)
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
      (is (= (value card) (pyramid-solver::card-value card))))))

(test cards-in-string
  (is (equal *ordered-deck* (pyramid-solver::cards-in-string "
            Ac M
          2c  3cC
        4c  5c  6ca
      7c  8c  9c  Tc0
    Jc  Qc  Kc  Ad  102d
  3d  4d  5d  6d  7d  ]8d
9d  Td  Jd  Qd  Kd  Ah  @2h
3h 4h 5h 6h 7h 8h 9h #Th JhQh Kh As 2s 3s 4s 5s 6s 7s 8s 9s Ts Js Qs Ks"))))

(test deckp
  (is (pyramid-solver::deckp *ordered-deck*))
  (is (not (pyramid-solver::deckp (rest *ordered-deck*))))
  (is (not (pyramid-solver::deckp (cons "Ks" *ordered-deck*))))
  (is (not (pyramid-solver::deckp (cons "as" *ordered-deck*))))
  (is (not (pyramid-solver::deckp (coerce *ordered-deck* 'vector))))
  (let ((dotted-list (copy-list *ordered-deck*)))
    (setf (cdr (last dotted-list 2)) "Ks")
    (is (not (pyramid-solver::deckp dotted-list)))))

(test calculate-pyramid-flags
  (let ((all-pyramid-flags (pyramid-solver::calculate-pyramid-flags)))
    (is (typep all-pyramid-flags 'vector))
    (is (= 1430 (length all-pyramid-flags)))
    (labels ((left-child (pyramid-index)
               (svref #(1 3 4 6 7 8 10 11 12 13 15 16 17 18 19 21 22 23 24 25 26) pyramid-index))
             (valid-pyramid-flags-p (pyramid-flags)
               (loop for i from 0 to 20
                     always (or (logbitp i pyramid-flags)
                                (zerop (mask-field (byte 2 (left-child i)) pyramid-flags))))))
      (is (every #'valid-pyramid-flags-p all-pyramid-flags)))))

(test calculate-pyramid-flags-to-id-mapping
  (is (loop with all-pyramid-flags = (pyramid-solver::calculate-pyramid-flags)
            with mapping = (pyramid-solver::calculate-pyramid-flags-to-id-mapping all-pyramid-flags)
            for pyramid-id from 0 below 1430
            for pyramid-flags = (svref pyramid-solver::*pyramid-flags* pyramid-id)
            always (= pyramid-id (gethash pyramid-flags mapping)))))

(test calculate-pyramid-indexes
  (is (loop for pyramid-flags across (pyramid-solver::calculate-pyramid-flags)
            for pyramid-indexes = (pyramid-solver::calculate-pyramid-indexes pyramid-flags)
            always (= pyramid-flags
                      (apply #'logior (loop for i in pyramid-indexes collect (ash 1 i)))))))