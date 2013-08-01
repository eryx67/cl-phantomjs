(in-package #:phantomjs)

(defvar *phantomjs-bin* #p"/usr/bin/phantomjs")
(defvar *debugp* t)
(defvar *phantomjs-default-args* (list "--debug=true"))
(defvar *cookies-file* (merge-pathnames #p".phjs.cookies"
                                        (user-homedir-pathname)))
(defvar *scripts-path* (merge-pathnames
                        #p"deps/js/"
                        (asdf:system-source-directory :phantomjs)))
(defvar *config-file* #p"config.js")
(defvar *response-prefix* "$phjs> ")
(defvar *server-file* #p"phjs-srv.js")

(defvar *instances* (make-hash-table))

(let ((port #x2fff))
  (defun next-portnum ()
    (incf port)
    (when (> port #xffff)
      (setf port #x2fff))
    port))

(defcondition* instance-error (error)
  ((message)
   (value))
  (:documentation "PhantomJS error.")
  (:export-accessor-names-p t)
  (:export-class-name-p t))

(defmethod print-object ((self instance-error) stream)
  (with-slots (message value) self
    (print-unreadable-object (self stream :type t :identity t)
      (format stream "~a ~@[~{~a => ~a~^, ~}~]~@[~a~]"
              message
              (when (hash-table-p value)
                (hash-table-plist value))
              (unless (hash-table-p value)
                value)))))

(defcondition* instance-call-error (instance-error)
  ((message)
   (value))
  (:documentation "Raised when javascript side returns error."))

(defun instance-sentinel (proc)
  (let ((status (external-program:process-status proc)))
    (when (not (eql :running status))
      (let ((pid (external-program:process-id proc)))
        (format t "Process phantomjs ~a finished with status ~a~%"
                pid status)
        (setf (proc-of (gethash pid *instances*)) nil)
        (remhash pid *instances*)))))

(defclass* phantomjs-instance ()
  ((proc :documentation "system process")
   (port :type integer :initform (next-portnum)))
  (:documentation "PhantomJS instance")
  (:export-accessor-names-p t)
  (:export-class-name-p t))

(defmethod instance-start ((self phantomjs-instance) &key cmd-args)
  (loop
     for startp = t then nil
     for port = (port-of self) then (next-portnum)
     for proc = (phantomjs-server port cmd-args)
     when (and (not startp) (= port (port-of self)))
     do (error 'instance-error :message "Can't allocate port")
     while (not proc)
     finally (setf (proc-of self) proc
                   (port-of self) port
                   (gethash (external-program:process-id proc) *instances*) self)))

(defmethod instance-receive ((self phantomjs-instance) (handler function)
                             &key (timeout 5) (oncep nil))
  (let ((in (external-program:process-output-stream (proc-of self))))
    (sb-sys:with-fd-handler ((sb-sys:fd-stream-fd in)
                             :input
                             #'(lambda (fd)
                                 (declare (ignore fd))
                                 (funcall handler
                                          (phantomjs-decode-response
                                           (phantomjs-read-response (proc-of self))))))
      (if oncep
          (sb-sys:serve-event timeout)
          (loop
             while (sb-sys:serve-event timeout))))))

(defmethod instance-stop ((self phantomjs-instance))
  (when-let (proc (proc-of self))
    (phantomjs-exit (port-of self))))

(defmethod instance-kill ((self phantomjs-instance))
  (when-let (proc (proc-of self))
    (external-program:signal-process proc :interrupt)))

(defmethod instance-alivep ((self phantomjs-instance))
  (when (proc-of self)
    t))

(defmethod instance-call ((self phantomjs-instance) script &key (errorp t))
  (multiple-value-bind (res validp) (phantomjs-call (port-of self) script)
    (if validp
        (values res validp)
        (if errorp
            (error 'instance-call-error :message "call error" :value res)
            (values res validp)))))

(defmethod inject-file ((self phantomjs-instance) file)
  (let ((path (namestring file)))
    (instance-call self (ps:ps (ps:chain phantom (inject-js (ps:lisp path)))))))

(defmethod inject-script ((self phantomjs-instance) (script string))
  (temporary-file:with-open-temporary-file (out :template "TEMPORARY-FILES:TEMP-%.js"
                                                :keep nil)
    (write-string script out)
    (finish-output out)
    (inject-file self (pathname out))))

(ps:defpsmacro phjs-send (msg)
  `(console.log (+ ,*response-prefix* (ps:chain (require "base64")
                                                (encode (ps:chain *json* (stringify ,msg)))))))

(defmacro wrap-ps (&body body)
  `(ps:ps (funcall (lambda () ,@body))))

(defmacro with-phantomjs ((var &key cmd-args) &body body)
  "Helper for PhantomJS.
  `VAR` -- PhantomJS instance variable.
  Special forms used in macro are `RECEIVE` and `CALL`.
  `CALL` -- (CALL VAR SCRIPT).
  `RECEIVE -- (RECEIVE (VAR MSG-VAR &KEY (TIMEOUT 5) (ONCEP NIL)) &BODY BODY)
  If not `ONCEP` body is called repeatedly before `TIMEOUT` occur.
  (RETURN-FROM-RECEIVE VAL) can be used in this case to exit receive loop."
  (once-only (cmd-args)
    (with-unique-names (srv)
      `(let* ((,srv (make-instance 'phantomjs-instance))
              (,var ,srv))
         (instance-start ,srv :cmd-args ,cmd-args)
         (macrolet ((call (srv script)
                      `(instance-call ,srv ,script))
                    (receive ((srv msg-var &rest args) &body body)
                      (with-unique-names (receive-block)
                        `(block ,receive-block
                           (flet ((return-from-receive (val)
                                    (return-from ,receive-block val)))
                             (instance-receive ,srv
                                               #'(lambda (,msg-var) ,@body)
                                               ,@args))))))
           (unwind-protect
                (progn
                  ,@body)
             (instance-stop ,srv)))))))

(defun phantomjs-server (port cmd-args)
  "Run a phantomjs process with a webserver on `PORT`.

The webserver running on `PORT` is used to send commands to the
phantomjs instance using a special protocol.

Returns `EXTERNAL-PROGRAM:PROCESS`."

  (let* ((start-args (append cmd-args
                             (list (format nil "--config=~a"
                                           (merge-pathnames *config-file*
                                                            *scripts-path*))
                                   (namestring (merge-pathnames *server-file*
                                                                *scripts-path*))
                                   (format nil "~a" port)
                                   (namestring (cl-fad:pathname-as-file *scripts-path*))))))
    (phantomjs-start start-args)))

(defun phantomjs-read-response (proc)
  (loop
     with prefix-len = (length *response-prefix*)
     with in = (external-program:process-output-stream proc)
     for str = (read-line in)
     for str-prefix = (when (> (length str) prefix-len)
                        (subseq str 0 prefix-len))
     when *debugp* do
       (format *error-output* "~&phantomjs=>~a~%" str)
     while (not (and str-prefix
                     (string-equal *response-prefix* str-prefix)))
     finally (return (subseq str prefix-len))))

(defun phantomjs-start (args)
  "Run phantomjs process. Returns `PROCESS`.
 SCRIPTS is a list of scripts to be passed to the process."
  (let* ((args (append
                (when *cookies-file
                  (list (format nil "--cookies-file=~a" *cookies-file*)))
                (append *phantomjs-default-args* args)))
         (proc (external-program:start *phantomjs-bin* args
                                       :status-hook #'instance-sentinel
                                       :input t
                                       :output :stream
                                       :error t)))
    (loop
       for resp = (phantomjs-read-response proc)
       while (not (member resp '("started" "failed") :test #'string-equal))
       finally (return (if (string-equal resp "started")
                           proc
                           (progn
                             (external-program:signal-process proc :interrupt)
                             nil))))))

(defun phantomjs-exec (port command &optional arg)
  "Call `COMMAND` with `ARG` in `PORT` of the phantomjs instance.
* `COMMAND` - :call | :exit.
Returns FIXME."
  (assert (member command '(:call :exit)))
  (if (member command '(:call))
      (unless arg
        (error "argument required for command ~a" command))
      (setf arg nil))
  (let ((headers (cons `("command" . ,(string-downcase command))
                       (when arg
                         (list `("commandarg" . ,arg))))))
    (multiple-value-bind (body status-code)
        (drakma:http-request (puri:parse-uri (format nil "http://localhost:~a" port))
                             :additional-headers headers)
      (let ((body-str (and body
                           (babel:octets-to-string body :encoding :utf-8))))
        (case status-code
          (200 (values body-str t))
          (t (values body-str nil))
          )))))

(defun phantomjs-call (port script)
  "Execute `SCRIPT` on `PORT` of the phantomjs instance."
  (multiple-value-bind (body validp) (phantomjs-exec port
                                                     :call
                                                     (phantomjs-encode-request script))
    (values (phantomjs-decode-response body) validp)))

(defun phantomjs-exit (port)
  "Exit the phantomjs instance on `PORT`."
  (phantomjs-exec port :exit))

(defun phantomjs-encode-request (str)
  (cl-base64:string-to-base64-string str))

(defun phantomjs-decode-response (str)
  (yason:parse (babel:octets-to-string (cl-base64:base64-string-to-usb8-array str))))
