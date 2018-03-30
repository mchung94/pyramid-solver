;;;; This is just some code to analyze results from running PS::RUN-DECKS.
;;;;
;;;; What PS::RUN-DECKS does is run every deck in a file, like
;;;; random-decks.txt, and print out the following for each deck as one list:
;;;; 1. The 52-card deck as a list of cards (two-letter strings).
;;;; 2. The solution as a list of steps consisting of either:
;;;;    - "Draw" - draw a card from the stock to the waste pile.
;;;;    - "Recycle" - recycle the waste pile back into the stock.
;;;;    - A list of one or two cards to remove, e.g. ("8c" "5d") or ("Kh").
;;;; 3. A counter starting from 1 for each deck/solution.
;;;; 4. A count in milliseconds (wall clock time) of how long it took to run.
;;;;
;;;; The code in this file is meant to be loaded and used interactively to
;;;; examine the results.  It can:
;;;; 1. Verify the solutions are playable.  It tries to play the game step by
;;;;    step using the solution and see if it makes any incorrect moves
;;;;    according to the rules, or if it doesn't result in actually clearing
;;;;    the board.
;;;; 2. Report timings.  It will report average, median, maximum, and total
;;;;    time for all decks, the decks where a solution is found, and the decks
;;;;    where a solution wasn't found.
;;;; 3. In the case of random-decks.txt, it will check with the other file
;;;;    random-deck-solution-lengths.txt to check that it found a solution with
;;;;    the same number of steps or NIL if there's no solution.
;;;;    random-deck-solution-lengths.txt was precalculated using a simple but
;;;;    slow breadth-first search to be certain the length is the minimum.
;;;;
;;;; Usage:
;;;; First, run LOAD-RESULTS with a filename.
;;;; Then run UNPLAYABLE-SOLUTIONS, REPORT-TIMINGS, and/or COMPARE-LENGTHS.

(defvar *results* nil
  "Results stored here - every other function refers to these results.
This is for convenience, we're intending to run this analysis interactively.")

(defun load-results (&optional (filename "run-decks-results.txt"))
  "Read the results from the filename into *RESULTS*."
  (with-open-file (in filename)
    (setf *results* (loop for result = (read in nil nil)
                          while result
                          collect result)))
  ;; don't return anything to avoid clutter in the REPL
  (values))

(defun result-deck (result)
  "Return the deck of cards used in the Pyramid Solitaire game result."
  (first result))

(defun result-solution (result)
  "Return the solution in the Pyramid Solitaire game result."
  (second result))

(defun result-count (result)
  "Return the game counter in the Pyramid Solitaire game result."
  (third result))

(defun result-time (result)
  "Return the time it took to solve the Pyramid Solitaire game."
  (fourth result))

(defvar *first-child-index*
  #(1 3 4 6 7 8 10 11 12 13 15 16 17 18 19 21 22 23 24 25 26)
  "For each pyramid card, the index of its first child.
The second is 1+ the first.  The bottom row of the pyramid has no entry here.")

(defun solution-playable-p (deck solution)
  "Return T if SOLUTION correctly wins Pyramid Solitaire using DECK."
  (when (null solution)
    (return-from solution-playable-p nil))
  (let ((pyramid (coerce (subseq deck 0 28) 'vector))
        (stock (subseq deck 28))
        (waste ())
        (cycle 1))
    (labels ((pyramid-index (card)
               "Return the card's index in the pyramid, or NIL if not found."
               (position card pyramid :test #'string=))
             (no-children-p (card-index)
               "Return T if CARD-INDEX has no card blocking it from below."
               (or (> card-index 20)
                   (let ((first-child (svref *first-child-index* card-index)))
                     (not (or (svref pyramid first-child)
                              (svref pyramid (1+ first-child)))))))
             (pyramid-uncovered-p (card)
               "Return T if the card is in the pyramid and is uncovered."
               (let ((index (pyramid-index card)))
                 (and index (no-children-p index))))
             (card-uncovered-p (card)
               "Return T if the card is uncovered, available for removal."
               (or (string= card (first stock))
                   (string= card (first waste))
                   (pyramid-uncovered-p card)))
             (card-king-p (card)
               "Return T if the card is a king."
               (char= (schar card 0) #\K)))
    (dolist (action solution)
      (cond ((and (equal action "Draw") stock)
             ;; draw a card from the stock pile to the waste pile
             (push (pop stock) waste))
            ((and (equal action "Recycle") (null stock) (< cycle 3) waste)
             ;; recycle the waste pile back into the stock pile and start the
             ;; next cycle
             (setf stock (reverse waste)
                   waste ())
             (incf cycle))
            ((and (listp action)
                  (every #'card-uncovered-p action)
                  (or (and (= 1 (length action))
                           (card-king-p (first action)))
                      (and (= 2 (length action))
                           (notany #'card-king-p action))))
             ;; remove the cards in the list
             (dolist (card action)
               (cond ((string= card (first stock)) (pop stock))
                     ((string= card (first waste)) (pop waste))
                     (t (setf (svref pyramid (pyramid-index card)) nil)))))
            (t
             ;; if no other condition applied then something's wrong with the
             ;; solution, it's unplayable
             (return-from solution-playable-p nil)))))
    (every #'not pyramid)))

(defun unplayable-solutions ()
  "Return the results with unplayable solutions."
  (if *results*
      (loop for result in *results*
            unless (or (null (second result))
                       (solution-playable-p (result-deck result)
                                            (result-solution result)))
            collect result)
    (format t "~&Please run LOAD-RESULTS first.~%")))

(defun all-times ()
  "Return all the times (in milliseconds) to solve every game."
  (mapcar #'result-time *results*))

(defun win-times ()
  "Return all the times (in milliseconds) to solve the winning games."
  (mapcar #'result-time (remove-if #'null
                                   *results*
                                   :key #'result-solution)))

(defun lose-times ()
  "Return all the times (in milliseconds) to check the unwinnable games."
  (mapcar #'result-time (remove-if (complement #'null)
                                   *results*
                                   :key #'result-solution)))

(defun sum (times)
  (reduce #'+ times))

(defun mean (times)
  (coerce (/ (sum times) (length times)) 'double-float))

(defun median (times)
  (let* ((sorted (sort (copy-seq times) #'<))
         (length (length times))
         (mid (floor length 2)))
    (if (evenp length)
        (mean (list (elt sorted mid) (elt sorted (1- mid))))
      (elt sorted mid))))

(defun maximum (times)
  (reduce #'max times))

(defun total (times)
  (multiple-value-bind (minutes seconds)
      (floor (/ (sum times) 1000.0) 60)
    ;; when minutes are single digits, pad it with 0 on the left
    (format nil "~2,,,'0@A:~2,,,'0@A" minutes seconds)))

(defun report-timings ()
  "Print all the relevant timings for the results."
  (if *results*
      (let ((times-list (list (all-times) (win-times) (lose-times))))
        (format t "~&All Times, Win Times, Lose Times~%")
        (format t "Mean: ~A~%" (mapcar #'mean times-list))
        (format t "Median: ~A~%" (mapcar #'median times-list))
        (format t "Maximum: ~A~%" (mapcar #'maximum times-list))
        (format t "Total: ~A" (mapcar #'total times-list)))
    (format t "~&Please run LOAD-RESULTS first.~%")))

(defun compare-lengths ()
  "Return results with non-optimal solutions if they come from random-decks.txt"
  (if *results*
      (with-open-file (in "random-deck-solution-lengths.txt")
        (loop for result in *results*
              for solution-length = (read in nil nil)
              unless (= solution-length (length (result-solution result)))
              collect result))
    (format t "~&Please run LOAD-RESULTS first.~%")))
