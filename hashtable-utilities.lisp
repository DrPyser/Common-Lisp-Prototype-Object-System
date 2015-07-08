(defpackage :hashtable-utilities
  (:nicknames :hash-table-utilities :hashtable-utils :ht-utils)
  (:use :cl))

(in-package :ht-utils)

(defun print-hashtable (hashtable &optional (stream t))
  (format stream "{")
  (with-hash-table-iterator (pair hashtable)
    (loop
       for (any-left k v) = (multiple-value-list (pair))
       while any-left
       do (format stream " ~A: ~A " k v)
       finally (format stream "}"))))

(defmethod print-object ((dict hash-table) stream)
  "method to print a hashtable in a JSON style"
  (print-hashtable dict stream))

(defun hashtable-values (hashtable)
  (let ((val Nil))
    (progn
      (maphash (lambda (k v)
		 (declare (ignore k))
		 (push v val)) hashtable)
      (reverse val))))

(defun hashtable-keys (hashtable)
  (let ((keys Nil))
    (progn
      (maphash (lambda (k v)
		 (declare (ignore v))
		 (push k keys)) hashtable)
      (reverse keys))))

(defun hashtable-pairs (hashtable)
  (let ((pairs Nil))
    (progn
      (maphash (lambda (k v)
		 (push (list k v) pairs)) hashtable)
      (reverse pairs))))

