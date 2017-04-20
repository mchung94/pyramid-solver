;;;; LispWorks delivery script
;;;; This probably needs changes to work for anyone else even if they are also
;;;; using LispWorks.  If any of the below assumptions are wrong it definitely
;;;; needs to change:
;;;; - You're loading Quicklisp in your user initialization file.
;;;; - The current directory has the .asd files for the project and it's okay
;;;;   to create a dist/ subdirectory.
;;;; - You're on Windows where (sys:call-system "rd /s /q dist") to remove
;;;;   an existing dist/ subdirectory will work.

;;; LispWorks user initialization file, mine doesn't do much other than
;;; load Quicklisp.
(load *init-file-name*)

;;; Create the distribution directory
;;; This looks weird but it helps me when I have DLLs, to copy them
;;; into dist/ and then run the build while inside dist/
(sys:call-system "rd /s /q dist")
(ensure-directories-exist "dist/")
(change-directory "dist/")

;;; Load the application
(push "../" asdf:*central-registry*)
(asdf:load-system :pyramid-solver)

;;; Deliver the application
(deliver
 'command-line:main
 "pyramid-solver"
 5
 :console t
 :keep-conditions :all)
