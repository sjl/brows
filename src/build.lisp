(ql:quickload :brows)

(setf deploy:*status-output* nil)

(let ((deploy:*status-output* t))
  (asdf:make :brows :force t))
