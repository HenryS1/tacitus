(defsystem "tacitus"
  :version "0.1.0"
  :author "Henry Steere"
  :license "MIT"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "tacitus"))))
  :description "Lazy indexable ranges for common lisp"
  :in-order-to ((test-op (test-op "tacitus/tests"))))

(defsystem "tacitus/tests"
  :author "Henry Steere"
  :license "MIT"
  :depends-on ("tacitus"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "tacitus"))))
  :description "Test system for tacitus"
  :perform (test-op (op c) (symbol-call :rove :run c)))
