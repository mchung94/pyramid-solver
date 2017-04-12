(defsystem #:pyramid-solver-tests
  :description "Tests for the Pyramid Solitaire solver"
  :depends-on (#:pyramid-solver
               #:fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "pyramid-solver"))))
  :perform (test-op (o s)
                    (symbol-call :fiveam '#:run-all-tests)))
