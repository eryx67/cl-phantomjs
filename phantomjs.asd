;;;; phantomjs.asd

(asdf:defsystem #:phantomjs
  :serial t
  :description "Interfacing with PhantomJS"
  :author "Vladimir G. Sekissov <eryx67@gmail.com>"
  :license "BSD"
  :depends-on (#:alexandria
               #:external-program
               #:drakma
               #:babel
               #:puri
               #:hu.dwim.defclass-star
               #:parenscript
               #:cl-fad
               #:cl-base64
               #:yason
               #:temporary-file)
  :components ((:file "package")
               (:file "external-program-fix")
               (:file "phantomjs")))
