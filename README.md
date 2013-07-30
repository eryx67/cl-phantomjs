# PhantomJS #

Helper classes for working with [PhantomJS](http://phantomjs.org)
and [CasperJS](http://http://casperjs.org/)

``` lisp
(let ((script (phjs:wrap-ps
                  (defvar casper (ps:chain (require "casper") (create)))
                  (defvar links (list))

                  (defun get-links ()
                    (defvar links ((ps:@ document query-selector-all) "h3.r a"))
                    (*array.prototype.map.call links
                                               (lambda (e)
                                                 ((ps:@ e get-attribute) "href"))))

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
                                 (setq links
                                       (links.concat (this.evaluate get-links)))
                                 (return links)))
                  (casper.run (lambda ()
                                (phjs:phjs-send
                                 (ps:create :found links.length :links links))))
                  (return t)
                  )))
    (phjs:with-phantomjs (srv)
      (phjs:call srv script)
      (phjs:receive (srv msg)
                    (phjs:return-from-receive (hash-table-plist msg)))))
;;=> ("links"
;;    ("/url?q=http://casperjs.org/&sa=..."
;;     "/url?q=http://docs.casperjs.org/&sa=..."
;;     ...
;;     "/url?q=http://docs.casperjs.org/en/latest/..."
;;     )
;;    "found" 19)
```
