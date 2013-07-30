(in-package :external-program)

#+sbcl
(defmethod process-id (process)
  (sb-ext:process-pid process))
