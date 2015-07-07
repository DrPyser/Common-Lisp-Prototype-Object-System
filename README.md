# Common-Lisp-Prototype-Object-System
## A prototype object system à la JavaScript, in Common Lisp.
An object is simply a hashtable. Keys can thus be keywords, symbols, strings or anything hashable(though keywords are prefered).
To replace dot notation, a general property accessor(getter/setter) and a method call operator are defined.

* function property: `(property object key [new-value])`
* function call: `(call object method-name {arguments})`

If 'property' is called with only the 'object' and 'key' argument, the object will be queried for a value associated to 'key'. If none is found inside the object's hashtable, and the 'object' has a value associated with the 'prototype' property, its prototype will be querried for the same key, and so on until the property is found or a prototype-less object is found. property queries return two values, the first being the property's value, and the second a boolean indicating whether the property has been found.
If the property function is given a third argument, the property 'key' will be set to the value 'new-value'(overwriting any existing value, or creating a new property).

The function 'call' is used to call methods on an object which also contains it. The object 'object' is queried for the property 'method-name'. If that property is defined, the returned value is assumed to be a function of at least one argument, the first of which is used to refer to the receiver of the method(its "target"). The object 'object' is passed as a first argument, followed by the rest of the arguments.

The macro 'func' serves as syntactic sugar for method definition, as a replacement of the 'lambda' macro. 'func' automatically adds a 'self' argument in front of the provided argument's list, and defines a local accessor function, 'self', that can be used to query and modify properties of the 'self' receiver object:

`(self key [value])`   
which is equivalent to
`(property self key [value])`.




