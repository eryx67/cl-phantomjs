(defpackage :phantomjs-test
  (:use #:cl #:fiveam)
  (:import-from #:alexandria
                #:hash-table-plist))

(in-package :phantomjs-test)

(def-suite :phantomjs)
(in-suite :phantomjs)

(test get-google
  (let ((script (phjs:wrap-ps
                  (defvar casper (ps:chain (require "casper") (create)))
                  (defvar links (list))

                  (defun get-links ()
                    (defvar links ((ps:@ document query-selector-all) "h3.r a"))
                    (*array.prototype.map.call links
                                               (lambda (e) ((ps:@ e get-attribute) "href"))))

                  (casper.start "http://google.fr"
                                (lambda ()
                                  (this.fill "form[action=\"/search\"]"
                                             (ps:create :q "casperjs")
                                             t)))
                  (casper.then (lambda ()
                                 (setq links (this.evaluate get-links))
                                 (this.fill "form[action=\"/search\"]"
                                            (ps:create :q "phantomjs")
                                            t)))
                  (casper.then (lambda ()
                                 (setq links (links.concat (this.evaluate get-links)))
                                 (return links)))
                  (casper.run (lambda ()
                                (phjs:phjs-send (ps:create :found links.length :links links))))
                  (return t)
                  )))
    (let ((res (phjs:with-phantomjs (srv)
                 (phjs:call srv script)
                 (phjs:receive (srv msg)
                               (phjs:return-from-receive msg)))))
      (is (numberp (gethash "found" res)))
      (is (= (gethash "found" res) (length (gethash "links" res))))
      )))
