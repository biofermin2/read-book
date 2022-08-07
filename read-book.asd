(defsystem "read-book"
  :version "0.1.0"
  :author "biofermin2"
  :license "MIT"
  :depends-on (:dexador :cl-ppcre :cl-interpol :cl-mecab :cl-rainbow)
  :components ((:module "src"
                :components
                ((:file "read-book"))))
  :description "this program support to read the article that mixed japanese and english sentences."
  :in-order-to ((test-op (test-op "read-book/tests"))))

(defsystem "read-book/tests"
  :author ""
  :license ""
  :depends-on ("read-book"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for read-book"
  :perform (test-op (op c) (symbol-call :rove :run c)))
