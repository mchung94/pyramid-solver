(in-package #:command-line)

(define-condition usage-error (error)
  ((message :reader message :initarg :message))
  (:report (lambda (condition stream)
             (format stream "~A" (message condition))))
  (:documentation "Usage errors are command line syntax errors."))

(define-condition deck-error (error)
  ((deck :reader deck :initarg :deck))
  (:report (lambda (condition stream)
             (format stream "The deck is not a standard deck: ~A." (deck condition))))
  (:documentation "Deck errors occur when the deck isn't a standard deck of cards."))

(defun print-usage (argv)
  "Print a usage message to *ERROR-OUTPUT*."
  (format *error-output* "Usage: ~A card-filename~%" (first argv)))

(defun print-solution (solution)
  "Print a Pyramid Soliatire solution given by PS:SOLVE."
  (if solution
      (progn
        (format t "~&There are ~D steps in the solution:~%" (length solution))
        (dolist (action solution)
          (format t "~A~%" (ps:human-readable-action action))))
    (format t "~&There is no way to clear all the pyramid cards.~%")))

(defun run (&optional argv)
  "The main function for the command line - safe for interactive use."
  (let ((exit-status 0))
    (unless argv
      (setf argv (uiop:raw-command-line-arguments)))
    (handler-case
        (if (/= 2 (length argv))
            (error 'usage-error :message "Wrong number of arguments.")
          (let ((deck (ps:string->card-list (uiop:read-file-string (elt argv 1)))))
            (unless (ps:standard-deck-p deck)
              (error 'deck-error :deck deck))
            (format t "~&Solving this deck:~%~A~%" (ps:deck->string deck))
            (print-solution (ps:solve deck))))
      (usage-error (err)
        (format *error-output* "~A~%" err)
        (print-usage argv)
        (setf exit-status 2))
      (deck-error (err)
        (format *error-output* "~A~%" err)
        (format *error-output* "Number of cards: ~A~%" (length (deck err)))
        (format *error-output* "Missing cards: ~A~%" (ps:missing-cards (deck err)))
        (format *error-output* "Duplicate cards: ~A~%" (ps:duplicate-cards (deck err)))
        (format *error-output* "Malformed cards: ~A~%" (ps:malformed-cards (deck err)))
        (setf exit-status 3))
      (error (err)
        (format *error-output* "~A~%" err)
        (setf exit-status 1)))
    exit-status))

(defun main ()
  "The main function for the executable - don't run interactively."
  (uiop:quit (run)))
