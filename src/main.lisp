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

(defun clamp (lo v hi)
  (max lo (min hi v)))

(defun incf-pos (delta)
  (setf *pos* (clamp 0 (+ *pos* delta) (1- (length *urls*)))))


;;;; Actions ------------------------------------------------------------------
(defclass action ()
  ((tty :initarg :tty :accessor tty)
   (keys :initarg :keys :accessor keys)
   (thunk :initarg :thunk :accessor thunk)))

(defun create-action (thunk keys tty)
  (let ((action (make-instance 'action :thunk thunk :keys keys :tty tty)))
    (dolist (key (alexandria:ensure-list keys))
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
       (when ,exit (throw 'done nil)))
     ,keys
     ,tty))

(defun perform-action (action)
  (funcall (thunk action) (aref *urls* *pos*))
  (boots:redraw :full (tty action)))


;;;; Input --------------------------------------------------------------------
(defun find-urls (string)
  (let ((matches (ppcre:all-matches-as-strings
                   *regex* string
                   ;; ccl can't take non-simple-strings as external program
                   ;; args, because fuck me
                   :sharedp nil)))
    (coerce (remove-duplicates matches :test #'string-equal) 'vector)))

(defun read-input (path)
  (if (equal "-" path)
    (read-standard-input-into-string)
    (alexandria:read-file-into-string path)))

(defun process-input (input)
  (find-urls input))


;;;; UI -----------------------------------------------------------------------
(defun draw (pad)
  (boots:draw pad 0 0 (format nil "brows v~A" *version*))
  (iterate
    (for y :from 2 :below (boots:height pad))
    (for url :in-vector *urls* :with-index i)
    (for selected = (= i *pos*))
    (when selected
      (boots:draw pad 0 y "-> " (boots:attr :bold t)))
    (boots:draw pad 3 y url (boots:attr :bold selected))))

(defun init ()
  (let ((*package* (find-package :brows)))
    (load "~/.browsrc" :if-does-not-exist nil))
  (setf *urls* (process-input (read-input "-"))))

(defun main ()
  (iterate
    (boots:redraw)
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
  (catch 'done
    (with-open-file (input "/dev/tty" :direction :input)
      (with-open-file (output "/dev/tty" :direction :output :if-exists :append)
        (boots/terminals/ansi:with-ansi-terminal (terminal :input-stream input :output-stream output)
          (boots:with-screen (screen terminal :root (boots:make-canvas :draw #'draw))
            (init)
            (main)))))))


;;;; CLI ----------------------------------------------------------------------
(adopt:define-string *documentation*
  "Brows is a utility for finding links in a chunk of text and presenting a ~
  nice text-based UI for opening them.  It's written (and customizable) in ~
  Common Lisp.")

(defparameter *ui*
  (adopt:make-interface
    :name "brows"
    :usage "[OPTIONS]"
    :summary "Find links and present a menu for opening them in a browser."
    :help *documentation*
    :examples '(("Present a menu for opening some links:" .
                 "curl http://stevelosh.com/ | brows"))
    :contents (list (adopt:make-option 'help
                      :help "Display help and exit."
                      :long "help"
                      :short #\h
                      :reduce (constantly t)))))

(defun toplevel ()
  (handler-case
      (multiple-value-bind (arguments options) (adopt:parse-options *ui*)
        (when (gethash 'help options)
          (adopt:print-help-and-exit *ui*))
        (when arguments
          (error "Unrecognized command line arguments: ~S" arguments))
        (run))
    (error (c) (adopt:print-error-and-exit c))))
