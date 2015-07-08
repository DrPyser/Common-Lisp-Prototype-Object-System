# Common-Lisp-Prototype-Object-System
## A prototype object system à la JavaScript, in Common Lisp.
An object is simply a hashtable, and properties and methods are simply key-value pairs. Keys can thus be keywords, symbols, strings or anything hashable(though keywords are prefered).
To replace dot notation, a general property accessor(getter/setter) and a method call operator are defined.

* `(property object key &optional new-value)`
* `(call object method-name &rest arguments)`

If 'property' is called with only the 'object' and 'key' argument, the object will be queried for a value associated to 'key'. If none is found inside the object's hashtable, and the 'object' has a value associated with the 'prototype' property, its prototype will be querried for the same key, and so on until the property is found or a prototype-less object is found. property queries return two values, the first being the property's value, and the second a boolean indicating whether the property has been found.
If the property function is given a third argument, the property 'key' in the object's hashtable will be set to the value 'new-value'(overwriting any existing value, or creating a new property).

The function 'call' is used to call a method on an object which also contains it. The object 'object' is queried for the property 'method-name'. If that property is defined, the returned value is assumed to be a function of at least one argument, the first of which is used to refer to the receiver of the method(its "target"). The object 'object' is passed as a first argument, followed by the rest of the arguments.

The macro `func` serves as syntactic sugar for method definition, as a replacement of the `lambda` macro. `func` automatically adds a `self` argument in front of the provided argument's list, and defines a local accessor function, `self`, that can be used to query and modify properties of the 'self' receiver object:  
`(func args &body body)`

`(self key &optional value)`   
which is equivalent to  
`(property self key &optional value)`.

A handful of other such functions are also defined:

* `(prototype-of object)`:returns the value of the 'prototype' property of 'object'.
* `(from-prototype object)`:creates and return an empty object that contains only a property 'prototype' set to 'object'.
* `(add-to-prototype object key value)`: adds property 'key' of value 'value' to the prototype of 'object'.
* `(set-prototype object prototype)`: set the prototype of 'object' to 'prototype'.
* `(make-object &key (prototype *root-object*) properties)`: allows quick object creation. 'properties' is a list of key-value pairs.
* `(call-from object method-name receiver &rest args)`: allows a method to be called from an object, but with a different receiver(i.e. 'self' value).
A reader macro, implemented in the `:JSON-syntax` package, allows creation of objects/hashtables with a JSON-like syntax:

`{(key value)}`  
which is equivalent to the standard common lisp code:  
```
(let ((object (make-hash-table)))
  (setf (gethash object key) value))
```
  
The root object `*root-object*` serves as a common ancestor to objects, and is used to provide basic utilities to every objects inheriting from it, through the prototype interface:

* `:to-string`:returns a string version of the receiver.
* `:print`: prints the receiver.
* `:hash`: provides a hash function for hashing the receiver
* `:has-own-propertyp key`: returns a boolean indicating whether the receiver possess the property 'key' in its own hashtable
* `:clone`: returns a clone(1 level deep) of the receiver
* `:equal`: provide an equality function for the receiver(defaults to `equal`).

Any property and method can be redefined by an inheriting object.

A constructor object, a `defconstructor` macro as well as a `new` function are also defined, allowing JavaScript-like object constructors. The `defconstructor` macro defines a constructor object and a constructor function, and puts both in the symbol-value and symbol-function slots of the constructor's name symbol. 

```
(defconstructor node (value &optional left right)
  (self :value value)
  (self :left left)
  (self :right right))
```
The constructor object contains informations such as the name, arguments, and body of the constructor function, a reference to the function itself, and a prototype property which is also the prototype of the constructors "offsprings". A new object is constructed from the constructor using the `new` function on the constructor object.

```
>(new node 1) 
=>{prototype: {prototype: *root-object*, constructor: {...}},
value: 1,left: nil, rigth: nil,left: nil, right: nil}
>(property (new node 1) :constructor)
=>{prototype:{prototype: *root-object*, 
              constructor: {...}}, 
              args: (value &optional left right), 
              body: ((self :value value) (self :left left) (self :right right)), 
              name: node, 
              constructor-function: #<CLOSURE (LAMBDA (SELF VALUE &OPTIONAL LEFT RIGHT)) {1006E1594B}>
} 
```
Finally, the `call-with` function allows the constructor to be called with a different `self` reference(e.g. inside another constructor).
```
(defconstructor indexed-node (key value &optional left right)
    (call-with node self value left right)
    (self :key key))
```
