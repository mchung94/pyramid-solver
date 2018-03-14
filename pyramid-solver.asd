(defsystem "pyramid-solver"
  :description "A Pyramid Solitaire solver"
  :author "Mitchell Chung"
  :license "MIT"
  :pathname "src/"
  :serial t
  :components
  ((:file "packages")
   (:file "pyramid-solver")
   (:file "command-line"))
  :in-order-to ((test-op (test-op "pyramid-solver-tests"))))
