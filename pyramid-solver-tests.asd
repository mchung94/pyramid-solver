(defsystem "pyramid-solver-tests"
  :description "Tests for the Pyramid Solitaire solver"
  :depends-on ("pyramid-solver" "fiveam")
  :pathname "t/"
  :components
  ((:file "pyramid-solver-tests"))
  :perform (test-op (o c) (symbol-call :fiveam '#:run-all-tests)))
