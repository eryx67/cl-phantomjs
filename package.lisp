;;;; package.lisp

(defpackage #:phantomjs
  (:nicknames #:phjs)
  (:use #:cl)
  (:import-from #:alexandria
                #:when-let
                #:if-let
                #:switch
                #:hash-table-plist
                #:with-unique-names
                #:once-only)
  (:import-from #:hu.dwim.defclass-star
                #:defclass*
                #:defcondition*)
  (:import-from #:parenscript
                #:*ps-print-pretty*)
  (:export #:*phantomjs-bin*
           #:*phantomjs-default-args*
           #:*cookies-file*
           #:*scripts-path*
           #:*config-file*
           #:instance-start
           #:instance-receive
           #:instance-stop
           #:instance-kill
           #:instance-alivep
           #:instance-call
           #:inject-file
           #:inject-script
           #:phjs-send
           #:wrap-ps
           #:with-phantomjs
           #:return-from-receive
           #:call
           #:receive))
