(in-package :brows)

(defparameter *regex*
  (ppcre:create-scanner
    ; the regex in urlview's docs, with added [] exclusion
    "(((http|https|ftp|gopher)|mailto):(//)?[^ \\[\\]<>\"\\t]*|(www|ftp)[0-9]?\\.[-a-z0-9.]+)[^ .,;\\[\\]\\t\\n\\r<\">\\):]?[^, <>\\[\\]\"\\t]*[^ .,;\\[\\]\\t\\n\\r<\">\\):]"
    :case-insensitive-mode t))


(defparameter *urls* nil)
(defparameter *log* nil)
(defparameter *pos* 0)
(defparameter *actions* (make-hash-table))


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

(defmacro define-action (keys program &key
                         (url 'url)
                         (arguments `(list ,url))
                         tty)
  (with-gensyms (action key)
    `(let ((,action (lambda (,url)
                      (external-program:run ,program ,arguments
                                            ,@(if tty '(:output t :input t) '())))))
       (dolist (,key (ensure-list ,keys))
         (setf (gethash ,key *actions*) ,action)))))

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
  (load "~/.browsrc" :if-does-not-exist nil)
  (setf *urls* (-<> "-"
                 read-input
                 process-input)))

(defun main ()
  (iterate
    (boots:blit)
    (for event = (boots:read-event))
    (for action = (gethash event *actions*))
    (if action
      (perform-action action)
      (case event
        ((#\Q #\q) (return-from main))
        ((#\k :up) (incf-pos -1))
        ((#\j :down) (incf-pos 1))
        (t (setf *log* event))))))

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

