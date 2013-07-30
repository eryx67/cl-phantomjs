;;;; phantomjs.asd

(asdf:defsystem #:phantomjs-test
  :serial t
  :description "Tests and examples for PHANTOMJS"
  :author "Vladimir G. Sekissov <eryx67@gmail.com>"
  :license "BSD"
  :depends-on (#:phantomjs #:fiveam #:parenscript #:alexandria)
  :components ((:file "phantomjs-test")))
