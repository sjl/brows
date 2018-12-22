(in-package :brows)

;;;; State --------------------------------------------------------------------
(defparameter *regex*
  (ppcre:create-scanner
    ;; https://gist.github.com/gruber/249502
    "(?i)\\b((?:[a-z][\\w-]+:(?:/{1,3}|[a-z0-9%])|www\\d{0,3}[.]|[a-z0-9.\-]+[.][a-z]{2,4}/)(?:[^\\s()<>]+|\\(([^\\s()<>]+|(\\([^\\s()<>]+\\)))*\\))+(?:\\(([^\\s()<>]+|(\\([^\\s()<>]+\\)))*\\)|[^\\s`!()\\[\\]{};:'\".,<>?«»“”‘’]))"
    :case-insensitive-mode t))

(defparameter *version* (asdf:component-version (asdf:find-system :brows)))
(defparameter *urls* nil)
(defparameter *pos* 0)
(defparameter *actions* (make-hash-table))


;;;; Utils --------------------------------------------------------------------
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

(defun quit (&optional code)
  #+sbcl (sb-ext:exit :code code)
  #+ccl (ccl:quit code)
  #+abcl (ext:quit :status code)
  #+ecl (ext:quit code)
  #-(or ccl sbcl ecl abcl)
  (error "QUIT not supported on this implementation"))


;;;; Actions ------------------------------------------------------------------
(defclass* (action :conc-name "") ()
  (tty keys thunk))

(defun create-action (thunk keys tty)
  (let ((action (make-instance 'action :thunk thunk :keys keys :tty tty)))
    (dolist (key (ensure-list keys))
      (setf (gethash key *actions*) action))))

(defmacro define-action (keys program &key
                         (url 'url)
                         (arguments `(list ,url))
                         exit
                         tty)
  `(create-action
     (lambda (,url)
       (external-program:run ,program ,arguments
                             ,@(if tty
                                 '(:output t :input t)
                                 '()))
       (when ,exit (quit)))
     ,keys
     ,tty))

(defun perform-action (action)
  (when (tty action)
    (charms/ll:endwin))
  (funcall (thunk action) (aref *urls* *pos*))
  (when (tty action)
    (boots:blit)))


;;;; Input --------------------------------------------------------------------
(defun find-urls (string)
  (-<> string
    (ppcre:all-matches-as-strings
      *regex* <>
      :sharedp nil) ; ccl can't take non-simple-strings as external program args, because fuck me
    (remove-duplicates <> :test #'string-equal)
    (coerce <> 'vector)))

(defun read-input (path)
  (if (equal "-" path)
    (read-standard-input-into-string)
    (read-file-into-string path)))

(defun process-input (input)
  (find-urls input))


;;;; UI -----------------------------------------------------------------------
(defun draw (canvas)
  (boots:clear canvas)
  (boots:draw canvas 0 0 (format nil "brows v~A" *version*))
  (iterate
    (for row :from 2 :below (boots:height canvas))
    (for url :in-vector *urls* :with-index i)
    (when (= i *pos*)
      (boots:draw canvas row 0 "-> "))
    (boots:draw canvas row 3 url)))

(defun init ()
  (let ((*package* (find-package :brows)))
    (load "~/.browsrc" :if-does-not-exist nil))
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
        (t nil)))))

(defun run ()
  (boots:with-boots (:fresh-tty t)
    (boots:with-layer ()
        (boots:canvas () #'draw)
      (init)
      (main))))


;;;; CLI ----------------------------------------------------------------------
(adopt:define-string *documentation*
  "Brows is a utility for finding links in a chunk of text and presenting a ~
  nice text-based UI for opening them.  It's written (and customizable) in ~
  Common Lisp.")

(adopt:define-interface *ui*
    (:name "brows"
     :summary "Find links and present a menu for opening them in a browser."
     :usage "[OPTIONS]"
     :documentation *documentation*
     :examples '(("Present a menu for opening some links:" .
                  "curl http://stevelosh.com/ | brows")))
  (help
    :documentation "display help and exit"
    :long "help"
    :short #\h
    :reduce (constantly t)))

(defun toplevel ()
  (handler-case
      (multiple-value-bind (arguments options) (adopt:parse-options *ui*)
        (when (gethash 'help options)
          (adopt:print-usage-and-exit *ui*))
        (unless (null arguments)
          (error "Unrecognized command line arguments: ~S" arguments))
        (run))
    (error (c) (adopt:print-error-and-exit c))))
