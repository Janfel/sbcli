#!/usr/bin/env -S sbcl --script

(load (format nil "~a/setup.lisp" (or (sb-ext:posix-getenv "QUICKLISP_HOME") #P"~/quicklisp/")))
(let ((*standard-output* (make-broadcast-stream)))
  (ql:quickload "alexandria")
  (ql:quickload "cl-readline"))

(defpackage :sbcli
  (:use :common-lisp :cffi)
  (:export sbcli +repl-name+ +repl-version+ *welcome-msg* *goodbye-msg* *prompt* *prompt2* *ret*
           +home-directory+ +init-file+ *hist-file* *special* *last-result*))

(defpackage :sbcli-user
  (:use :common-lisp :sbcli))

(in-package :sbcli)

(defconstant +repl-name+    "Veit's REPL for SBCL")
(defconstant +repl-version+ "0.1.3")
(defconstant +home-directory+
  (format nil "~a/sbcli/" (or (sb-ext:posix-getenv "XDG_CONFIG_HOME") #P"~/.config/"))
  "The directory where the init and history files are stored.")
(defconstant +init-file+
  (merge-pathnames "init.lisp" +home-directory+)
  "The lisp file that is loaded on startup.")

(defvar *welcome-msg* (format nil "~a version ~a~%" +repl-name+ +repl-version+))
(defvar *goodbye-msg* "Bye for now.")
(defvar *prompt*      "sbcl> ")
(defvar *prompt2*     "....> ")
(defvar *ret*         "=> ")
(defvar *hist-file*   (merge-pathnames "history" +home-directory+))
(defvar *last-result* nil)
(defvar *hist*        nil)
(declaim (special *special*))

(defun read-hist-file ()
  (ensure-directories-exist *hist-file*)
  (with-open-file (in *hist-file* :if-does-not-exist :create)
    (loop for line = (read-line in nil nil)
       while line
       ;; hack because cl-readline has no function for this. sorry.
       do (cffi:foreign-funcall "add_history"
                                :string line
                                :void))))

(defun update-hist-file (str)
  (ensure-directories-exist *hist-file*)
  (with-open-file (out *hist-file*
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (write-line str out)))

(defun end ()
  "Ends the session"
  (write-line *goodbye-msg*)
  (sb-ext:quit))

(defun reset ()
  "Resets the session environment"
  (delete-package 'sbcli)
  (defpackage :sbcli (:use :common-lisp))
  (in-package :sbcli))

(defun split (str chr)
  (loop for i = 0 then (1+ j)
     as j = (position chr str :start i)
     collect (subseq str i j)
     while j))

(defun join (str chr)
  (reduce (lambda (acc x)
            (if (zerop (length acc))
                x
                (concatenate 'string acc chr x)))
          str
          :initial-value ""))

(defun novelty-check (str1 str2)
  (string/= (string-trim " " str1)
            (string-trim " " str2)))

(defun add-res (txt res) (setq *hist* (cons (list txt res) *hist*)))

(defun format-output (&rest args)
  (format (car args) "~a ; => ~a" (caadr args) (cadadr args)))

(defun write-to-file (fname)
  "Writes the current session to a file <filename>"
  (with-open-file (file fname
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
    (format file "~{~/sbcli:format-output/~^~%~}" (reverse *hist*))))

(defun help (sym)
  "Gets help on a symbol <sym>"
  (handler-case (inspect (read-from-string sym))
    (error (c) (format *error-output* "Error during inspection: ~a~%" c))))

(defun general-help ()
  "Prints a general help message"
  (format t "~a version ~a~%" +repl-name+ +repl-version+)
  (write-line "Special commands:")
  (maphash
   (lambda (k v) (format t "  ~a~c~a~%" k #\tab (documentation (cdr v) t)))
   *special*))

(defun print-defined-symbols ()
  "Lists all symbols defined during this session"
  (write-line "Currently defined:")
  (do-all-symbols (sym *package*)
    (when (eql (symbol-package sym) *package*)
      (if (boundp sym)
          (format t "  ~a~c(~a) ~a = ~a~%"
                  (string-downcase (string sym))
                  #\tab
                  (if (constantp sym) 'constant 'variable)
                  (or (documentation sym 'variable)
                      "No documentation")
                  (eval sym)))
      (if (fboundp sym)
          (format t "  ~a~c(FUNCTION) ~a~%"
                  (string-downcase (string sym))
                  #\tab
                  (or (documentation sym 'function)
                      "No documentation"))))))

(defun dump-disasm (sym)
  "Dumps the disassembly of a symbol <sym>"
  (handler-case (disassemble (read-from-string sym))
    (unbound-variable              (var) (format t "~a~%" var))
    (type-error                    (err) (format t "~a~%" err))
    (sb-int:compiled-program-error (err) (format t "~a~%" err))
    (undefined-function            (fun) (format t "~a~%" fun))))

(defun dump-type (expr)
  "Prints the type of a expression <expr>"
  (handler-case (format t "~a~%" (type-of (eval (read-from-string expr))))
    (unbound-variable              (var) (format t "~a~%" var))
    (type-error                    (err) (format t "~a~%" err))
    (sb-int:compiled-program-error (err) (format t "~a~%" err))
    (undefined-function            (fun) (format t "~a~%" fun))))

(defun custom-complete (text start end)
  (declare (ignore start) (ignore end))
  (labels ((common-prefix (items)
             (let ((lst 0))
               (loop for n from 1 below (reduce #'min (mapcar #'length items)) do
                    (when (every (lambda (x)
                                   (char= (char (car items) n)
                                          (char x           n)))
                                 (cdr items))
                      (setf lst n)))
               (write lst)
               (subseq (car items) 0 (+ lst 1))))
           (starts-with (text)
             (lambda (sym)
               (let* ((symstr (string-downcase sym))
                      (cmp (subseq symstr 0 (min (length symstr) (length text)))))
                 (string= text cmp))))
           (select-completions (list)
             (let* ((els (remove-if-not (starts-with text)
                                        (mapcar #'string list)))
                    (els (if (cdr els) (cons (common-prefix els) els) els)))
               (if (string= text (string-downcase text))
                   (mapcar #'string-downcase els)
                   els)))
           (get-all-symbols ()
             (let ((lst ()))
               (do-all-symbols (s lst)
                 (when (or (fboundp s) (boundp s)) (push s lst)))
               lst)))
    (select-completions (get-all-symbols))))

(rl:register-function :complete #'custom-complete)

;; -1 means take the string as one arg
(defvar *special*
  (alexandria:alist-hash-table
   `(("h"    . (1  . ,#'help))
     ("help" . (0  . ,#'general-help))
     ("def"  . (0  . ,#'print-defined-symbols))
     ("s"    . (1  . ,#'write-to-file))
     ("d"    . (1  . ,#'dump-disasm))
     ("t"    . (-1 . ,#'dump-type))
     ("q"    . (0  . ,#'end))
     ("r"    . (0  . ,#'reset))) :test 'equal))

(defun sbcli (txt p)
  (let ((text
         (rl:readline :prompt (if (functionp p) (funcall p) p)
                      :add-history t
                      :novelty-check #'novelty-check)))
    (in-package :sbcli-user)
    (if (not text) (end))
    (if (string= text "") (sbcli "" *prompt*))
    (when *hist-file* (update-hist-file text))
    (cond
      ((and (> (length text) 1) (string= (subseq text 0 1) ","))
       (let* ((splt (split text #\Space))
              (k (subseq (car splt) 1 (length (car splt))))
              (v (gethash k *special*)))
         (if (not v)
             (format *error-output* "Unknown special command: ~a~%" k)
             (let ((l (car v))
                   (rl (length (cdr splt))))
               (cond
                 ((= -1 l) (apply (cdr v) (list (join (cdr splt) " "))))
                 ((< rl l)
                  (format *error-output*
                          "Expected ~a arguments to ~a, but got ~a!~%"
                          l (car splt) rl))
                 (t (apply (cdr v) (subseq (cdr splt) 0 (car v)))))))))
      (t
       (let* ((new-txt (format nil "~a ~a" txt text))
              (parsed (handler-case (read-from-string new-txt)
                        (end-of-file () (sbcli new-txt *prompt2*))
                        (error (condition)
                          (format *error-output* "Parser error: ~a~%" condition)))))
         (if parsed
             (progn
               (setf *last-result*
                     (handler-case (eval parsed)
                       (unbound-variable (var) (format *error-output* "~a~%" var))
                       (undefined-function (fun) (format *error-output* "~a~%" fun))
                       (sb-int:compiled-program-error ()
                         (write-line "Compiler error." *error-output*))
                       (error (condition)
                         (format *error-output* "Evaluation error: ~a~%" condition))))
               (add-res text *last-result*)
               (if *last-result* (format t "~a~a~%" *ret* *last-result*)))))))
    (in-package :sbcli)
    (finish-output nil)
    (sbcli "" *prompt*)))

(cond
  ((probe-file +init-file+)    (load +init-file+))     ; .config/sbcli/init.lisp
  ((probe-file #P"~/.sbclirc") (load #P"~/.sbclirc"))) ; legacy init file

(write-line *welcome-msg*)
(write-line "Press CTRL-C or CTRL-D or type ,q to exit")
(terpri)
(finish-output nil)

(when *hist-file* (read-hist-file))

(handler-case (sbcli "" *prompt*)
  (sb-sys:interactive-interrupt () (end)))
