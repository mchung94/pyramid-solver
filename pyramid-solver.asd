(defsystem "pyramid-solver"
  :description "A Pyramid Solitaire (card game) solver."
  :author "Mitchell Chung"
  :license "MIT"
  :pathname "src/"
  :components ((:file "pyramid-solver"))
  :in-order-to ((test-op (test-op "pyramid-solver/tests"))))

(defsystem "pyramid-solver/tests"
  :description "Tests for pyramid-solver."
  :author "Mitchell Chung"
  :license "MIT"
  :depends-on ("pyramid-solver" "1am")
  :pathname "tests/"
  :components ((:file "pyramid-solver-tests"))
  :perform (test-op (o c) (symbol-call :1am '#:run)))
