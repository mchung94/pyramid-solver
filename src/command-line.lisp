(in-package #:command-line)

(defun print-condition-message (condition stream)
  (format stream "~A" (message condition)))

(define-condition usage-error (error)
  ((message :reader message
            :initarg :message))
  (:report print-condition-message)
  (:documentation "Usage errors are command line syntax errors."))

(define-condition deck-error (error)
  ((message :reader message
            :initarg :message))
  (:report print-condition-message)
  (:documentation "Something is wrong with the deck string cards."))

(defun print-usage (argv)
  "Print a usage message to *ERROR-OUTPUT*."
  (format *error-output* "Usage: ~A card-filename~%" (first argv)))

(defun run-deck (deck)
  "Run the solver on the given deck and print out the results or signal errors."
  (multiple-value-bind (is-deck num-cards missing dupes malformed)
      (ps:deckp deck)
    (when malformed
      (error 'deck-error
             :message (format nil "Malformed Cards: ~A" malformed)))
    (when missing
      (error 'deck-error
             :message (format nil "Missing Cards: ~A" missing)))
    (when dupes
      (error 'deck-error
             :message (format nil "Duplicate Cards: ~A" dupes)))
    (when (/= 52 num-cards)
      (error 'deck-error
             :message (format nil "~D cards in file, not 52" num-cards)))
    (when is-deck
      (let ((solution (ps:solve deck)))
        (if solution
            (progn
              (format t "~&~D steps in the solution:~%" (length solution))
              (loop for action in solution
                    do (format t "~&~A~%" (ps:human-readable-action action))))
          (format t "~&No solution exists.~%"))))))
             

(defun run (&optional argv)
  "The main function for the command line - safe for interactive use."
  (unless argv
    (setf argv (uiop:raw-command-line-arguments)))
  (handler-case
      (progn
        (when (/= (length argv) 2)
          (error 'usage-error :message "Wrong number of arguments."))
        (run-deck (ps:string->deck (uiop:read-file-string (elt argv 1)))))
    (usage-error (err)
      (format *error-output* "~A~%" err)
      (print-usage argv)
      (return-from run 2))
    (deck-error (err)
      (format *error-output* "Problem found with cards in ~A: ~A~%"
              (elt argv 1) err)
      (return-from run 1))
    (error (err)
      (format *error-output* "~A~%" err)
      (return-from run 1)))
  (return-from run 0))

(defun main ()
  "The main function for the executable - don't run interactively."
  (uiop:quit (run)))
