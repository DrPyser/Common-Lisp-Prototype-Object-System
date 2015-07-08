(load "~/Documents/Programmes/Common Lisp/utilities/hashtable-utilities.lisp")
(load "~/Documents/Programmes/Common Lisp/prototype-object-system/json-object-syntax.lisp")

;(load "~/Documents/Programmes/Common Lisp/syntax/json-syntax.lisp")
#|
This is an implementation of a prototype-based object system. In this system, "objects" are simply hashtable, properties being key-value pairs.

By covention, keywords are use for property keys, but they can be symbols or strings as well(though they are not interchangeable).

A "method" is defined using the 'func' macro. This macro replaces the 'lambda' macro to create functions, and has the same syntax.

The 'func' macro expands into a lambda with an additional first argument for the receiver object,
a let-binding of 'this' to the object passed as argument,
and a 'this' function (this property &optional value) which allows read/write access to properties of the receiver

Calling a method is done by using the function 'call' with the receiver and dispatch source as the first argument,
the method's key as a second,and the rest of the method's arguments as the remaining arguments: (call object method-name &rest args).

Lookup of a property is performed by first searching for the property in the object's own namespace.
If that fails and te object has no prototype object, the lookup fails. If the object has a prototype,
the same operation is performed on the prototype. So the chain of prototypes is climbed until the root object is reached.
If none of the original object's ancestors possess a property of the same name(key), the lookup fails.
When an ancestor does possess that property, its value is returned.

The root object *root-object* is the default prototype ancestor of every other object.
As such, it contains basic utility methods and properties, such as a clone, print, to-string, hash and equality functions..
|#

(in-package :prototype-object-system)

(setf *print-circle* t)

(enable-dictionary-syntax)  

(defun pprint-object (object s)
  (format s "{")
  (with-hash-table-iterator (pair object)
    (do () (nil) 
      (multiple-value-bind (any-left k v) (pair)
	(unless any-left
	  (format s "}")
	  (return))
	(if (eq k :prototype)
	    (if (null v)
		(format s " ~a: NIL~%" k)
		(format s " ~a: {...}~%" k))
	    (format s " ~A: ~A~%" k v))))))
			       
(defun copy (object)
  "Returns a copy of a common lisp entity, according to its type"
  (typecase object
    (list (copy-list object))
    (sequence (copy-seq object))
    (structure-object (copy-structure object))
    (t object)))
    
(defun clone (object)
  "Default function to clone an object, returning a 'deep' copy."
  (let ((copy (make-hash-table :test (hash-table-test object)
			       :size (hash-table-size object)
			       :rehash-size (hash-table-rehash-size object)
			       :rehash-threshold (hash-table-rehash-threshold object))))
    (prog1 copy
      (maphash (lambda (k v) (setf (gethash k copy) (copy v))) object))))

(defun to-string (object)
  "Default method to obtain a string representation of an object"
  (with-output-to-string (str)
    (format str "{")
    (with-hash-table-iterator (pair object)
      (do () (nil) 
	(multiple-value-bind (any-left k v) (pair)
	  (unless any-left
	    (format str "}")
	    (return str))
	  (if (and v (eq k :prototype))
	      (format str " ~A: ~A~%" k (to-string v))
	      (format str " ~A: ~A~%" k v)))))))

(defun property (object key &optional (new-value nil providedp))
  "General getter/setter. If 'new-value' is not provided, tries and lookup the key 'key' and return its value. "
  (if providedp
      (prog1 object
	(setf (gethash key object) new-value))
      (multiple-value-bind (value hit) (gethash key object)
	(if hit
	    (values value hit)
	    (multiple-value-bind (prototype hit) (gethash :prototype object)
	      (if hit
		  (property prototype key)
		  (values Nil Nil)))))))

(defmacro func (args &body body)
  "Defines a method, i.e. an anonymous function that has an object in its lexical scope.
The local function '(this property &optional value)' can be used as a getter/setter to this object(providing a value argument set the property to the value, otherwise the current value of the property is returned)"
  `(lambda (self . ,args)
     (declare (ignorable self))
     (flet ((self (key &optional (value nil providedp)) (if providedp
							    (property self key value)
							    (property self key))))
       ,@body)))


(defparameter *root-object* {
  (:prototype nil)
  (:print #'pprint-object)
  (:constructor nil)
  (:clone #'clone)
  (:to-string #'to-string)
  (:hash #'sxhash)
  (:equal #'equal)
  (:has-own-propertyp (func (property) (nth-value 1 (self property))))
  }
  "This is the root object. Default methods are stored here.")


(defmethod print-object ((object hash-table) stream)
  (multiple-value-bind (function hit) (property object :print)
    (if hit
	(funcall function object stream)
	(hashtable-utils::print-hashtable object stream))))

(defun prototype-of (object)
  "Returns the value of the :prototype property"
  (gethash :prototype object))

(defun has-own-propertyp (object property)
  "An object is said to have a property of its own if the object has a value associated to that property in its own hashtable."
  (nth-value 1 (gethash property object)))

(defun set-prototype (object prototype)
  "Sets the :prototype property of 'object' to 'prototype'."
  (property object :prototype prototype))

(defun ex-nihilo (&rest properties)
  "Creates a new object whose prototype is *root-object*, with initial properties defined by key-value pairs."
  (let ((new (from-prototype *root-object*)))
    (dolist (pair properties new)
      (destructuring-bind (key value) pair
	(property new key value)))))

(defun from-prototype (object)
  "Creates a new object whose :prototype property is set to 'object'."
  {(:prototype object)})

(defun get-keys (object)
  (hashtable-utils::hashtable-keys object))

(defun get-values (object)
  (hashtable-utils::hashtable-values object))

(defun call (object method-name &rest args)
  "Lookup the property 'method-name' in 'object', and call it with 'object' and 'args' as its arguments."
  (apply (property object method-name) object args))

(defun call-from (object method-name receiver &rest args)
  "Lookup property 'method-name' in 'object', 
and calls it with 'receiver' and 'args' as its arguments."
  (apply (property object method-name) receiver args))

(defun add-to-prototype (object key value)
  "Adds property 'property' with value 'value' to the prototype of 'object'."
  (property (prototype-of object) key value))		  

(defmacro defconstructor (name args &body body)
  "Defines a constructor object and a constructor function, each respectively bound to the symbol-value and symbol-function of the 'name' symbol.
The constructor function creates an object, which can then be defined by the 'body' of the function, and finally return that object, with its prototype set to the constructor object's prototype."
  `(let ((constructor-object (make-object)))
     (property constructor-object :name ',name)
     (property constructor-object :args ',args)
     (property constructor-object :body ',body)
     (property constructor-object :documentation ,(if (stringp (car body)) (car body)))
     (property constructor-object :constructor-function
	       (lambda ,`(self . ,args)
		 (flet ((self (key &optional (value nil valuep))
			  (when valuep
			    (property self key value))
			  (property self key)))
		   ,@body
		   (set-prototype self (prototype-of constructor-object)))))
     
     (set-prototype constructor-object (make-object :properties (list (list :constructor constructor-object))))
     (setf (symbol-value ',name) constructor-object)
     (setf (symbol-function ',name) (property constructor-object :constructor-function))
     constructor-object))

(defun new (constructor-object &rest args)
  "Creates a new object with the constructor 'constructor-object'"
  (let ((self (from-prototype (prototype-of constructor-object))))
    (prog1 self
      (apply (property constructor-object :constructor-function) self args))))

(defun call-with (constructor-object receiver &rest args)
  (prog1 receiver
    (apply (property constructor-object :constructor-function) receiver args)))

(defun constructor (object)
  (property object :constructor))

