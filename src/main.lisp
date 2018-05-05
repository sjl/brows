(in-package :brows)


(defparameter *regex*
  (concatenate
    'string
    "(((http|https|ftp|gopher)|mailto):(//)?[^ <>\"\\t]*|(www|ftp)[0-9]?\\.[-a-z0-9.]+)"
    "[^ .,;\\t\\n\\r<\">\\):]?[^, <>\"\\t]*[^ .,;\\t\\n\\r<\">\\):]"))

(defparameter *urls* nil)
(defparameter *log* nil)
(defparameter *pos* 0)


(defun find-urls (string)
  (-<> string
    (ppcre:all-matches-as-strings
      *regex* <>
      :sharedp nil) ; ccl can't take non-simple-strings as external program args, because fuck me
    (remove-duplicates <> :test #'string-equal)
    (coerce <> 'vector)))

(defun read-standard-input-into-string ()
  (with-output-to-string (result)
    (let* ((buffer-size 4096)
           (buffer (make-array buffer-size :element-type 'character)))
      (iterate
        (for bytes-read = (read-sequence buffer *standard-input*))
        (write-sequence buffer result :start 0 :end bytes-read)
        (while (= bytes-read buffer-size))))))

(defun incf-pos (delta)
  (setf *pos* (clamp 0 (1- (length *urls*))
                     (+ *pos* delta))))

(defun read-input (path)
  (if (equal "-" path)
    (read-standard-input-into-string)
    (read-file-into-string path)))

(defun process-input (input)
  (find-urls input))

(defun action-open (url)
  (external-program:run "open" (list url)))

(defun action-w3m (url)
  (external-program:run "w3m" (list url) :output t :input t))

(defun perform-action (action)
  (charms/ll:endwin)
  (funcall action (aref *urls* *pos*))
  (boots:blit))

(defun draw (canvas)
  (boots:clear canvas)
  (boots:draw canvas 0 0 (structural-string *log*))
  (iterate
    (with selected = (1+ *pos*))
    (for row :from 1 :below (boots:height canvas))
    (for url :in-vector *urls*)
    (when (= row selected)
      (boots:draw canvas row 0 "-> "))
    (boots:draw canvas row 3 url)))

(defun init ()
  (setf *urls* (-<> "-"
                 read-input
                 process-input)))

(defun main ()
  (iterate
    (boots:blit)
    (for event = (boots:read-event))
    (case event
      (#\newline (perform-action #'action-open))
      (#\w (perform-action #'action-w3m))
      ((#\Q #\q) (return-from main))
      ((#\k :up) (incf-pos -1))
      ((#\j :down) (incf-pos 1))
      (t (setf *log* event)))))

(defmacro catch-and-spew-errors (&body body)
  `(handler-case (progn ,@body)
     (t (c) (format t "Error: ~A" c))))

(defun toplevel ()
  (catch-and-spew-errors
    (boots:with-boots (:fresh-tty t)
      (boots:with-layer ()
          (boots:canvas () #'draw)
        (init)
        (main)))))

