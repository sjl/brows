(ql:quickload :brows :silent t)

(with-open-file (stream "bin/brows.1" :direction :output :if-exists :supersede)
  (adopt:print-manual brows::*ui* :stream stream))

(setf deploy:*status-output* nil)

(let ((deploy:*status-output* t))
  (asdf:make :brows :force t))
