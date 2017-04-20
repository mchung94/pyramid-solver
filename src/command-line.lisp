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

(defun run (&optional argv)
  "The main function for the command line - safe for interactive use."
  (unless argv
    (setf argv (uiop:raw-command-line-arguments)))
  (handler-case
      (progn
        (when (/= (length argv) 2)
          (error 'usage-error :message "Wrong number of arguments."))
        (let* ((filename (elt argv 1))
               (deck-string (uiop:read-file-string filename))
               (malformed-card (ps:find-malformed-card deck-string))
               (missing-cards (ps:missing-cards deck-string))
               (num-cards (ps:num-cards deck-string)))
          (format t "Running Pyramid Solver with the file ~A...~%" filename)
          (when malformed-card
            (error 'deck-error
                   :message (format nil "Malformed card: ~A" malformed-card)))
          (when missing-cards
            (error 'deck-error
                   :message (format nil "Missing cards: ~A" missing-cards)))
          (when (/= 52 num-cards)
            (error 'deck-error
                   :message (format nil "~A cards in the file, not 52"
                                    num-cards)))
          (let ((solution (ps:solve deck-string)))
            (if solution
                (progn
                  (format t "~A steps in the solution:~%" (length solution))
                  (loop for action in solution
                        do (format t "~A~%" (ps:human-readable-action action))))
              (format t "No solution exists.~%")))))
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
