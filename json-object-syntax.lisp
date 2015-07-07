(load "~/Documents/Programmes/Common Lisp/prototype-object-system/prototype-object-system-package.lisp")
(in-package :prototype-object-system)

(defvar *readtables-stack* nil)

(defun read-next-object (delimiter &optional (stream *standard-input*))
  (if (and delimiter (char= (peek-char t stream t Nil t) delimiter))
      (progn (read-char stream t nil t)
	     Nil)
      (let* ((object (read-preserving-whitespace stream t nil t))
	     (next-char (peek-char t stream t Nil t)))
	(cond ((and delimiter (char= next-char delimiter)) Nil)
					;((char= next-char #\space) (read-char stream t Nil t))
					;(t (error "Unexepected char: ~S" next-char))
	      )
	object)))
      
(defun read-delimiter (stream char)
  (declare (ignore stream))
  (error "Delimiter ~S is not meant to be read alone" char))

(defun create-hash-table (&rest pairs)
  (let ((hashtable (make-hash-table :test #'equal)))
    (dolist (pair pairs hashtable)
      (destructuring-bind (key value) pair
	(setf (gethash key hashtable) value)))))

(defun read-left-brace (stream char)
  (declare (ignore char))
  (loop
     for pair = (read-next-object #\} stream)
     while pair
     collect `(list ,@pair) into pairs
     finally (return `(create-hash-table ,@pairs))))

(defmacro enable-dictionary-syntax ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (push *readtable* *readtables-stack*)
     (setq *readtable* (copy-readtable))
     (set-macro-character #\{ #'read-left-brace)
     (set-macro-character #\} #'read-delimiter)))

(defmacro disable-dictionary-syntax ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (if (null *readtables-stack*)
	(error "There is no readtable on the stack")
	(setq *readtable* (pop *readtables-stack*)))))
