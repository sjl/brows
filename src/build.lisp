(ql:quickload :brows :silent t)

(with-open-file (stream "bin/brows.1" :direction :output :if-exists :supersede)
  (adopt:print-manual brows::*ui* :stream stream))

#+sbcl
(progn
  (sb-ext:gc :full t)
  (sb-ext:save-lisp-and-die
    "brows"
    :executable t
    :compression nil
    :toplevel #'brows:toplevel
    :save-runtime-options t))

#+ccl
(progn
  (ccl:gc)
  (ccl:save-application
    "brows"
    :toplevel-function #'brows:toplevel
    :purify t
    :prepend-kernel t))

#-(or sbcl ccl)
(error "Don't know how to build in this implementation.")
