(in-package :brows)



(defparameter *regex*
  (concatenate
    'string
    "(((http|https|ftp|gopher)|mailto):(//)?[^ <>\"\\t]*|(www|ftp)[0-9]?\\.[-a-z0-9.]+)"
    "[^ .,;\\t\\n\\r<\">\\):]?[^, <>\"\\t]*[^ .,;\\t\\n\\r<\">\\):]"))

(defparameter *urls* nil)
(defparameter *pos* 0)


(defun find-urls (string)
  (-<> string
    (ppcre:all-matches-as-strings *regex* <> :sharedp t)
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

(defun draw (canvas)
  (boots:clear canvas)
  (iterate (for row :from 0 :below (boots:height canvas))
           (for url :in-vector *urls*)
           (when (= row *pos*)
             (boots:draw canvas row 0 "-> "))
           (boots:draw canvas row 3 url)))

(defun init ()
  (setf *urls* (-<> "-"
                 read-input
                 process-input)))

(defun main ()
  (iterate
    (boots:blit)
    (case (boots:read-event)
      ((#\Q #\q) (return-from main))
      ((#\k :up) (incf-pos -1))
      ((#\j :down) (incf-pos 1)))))

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

