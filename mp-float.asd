(defsystem "mp-float"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "mp-float/tests"))))

(defsystem "mp-float/tests"
  :author ""
  :license ""
  :depends-on ("mp-float"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for mp-float"
  :perform (test-op (op c) (symbol-call :rove :run c)))
