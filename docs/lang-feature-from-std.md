# fff-lang Designment Document

## Language feature concerns from stdlib implementation

trying to write first version of stdlib, not only an integrated test of  
lexical parse and syntax parse, but also testing exist feature's rationality  
and new feature required

### Core feature requirements

  - var-def: variable definition, static type
  - int-type: primitive integral type
    include int (signed and unsigned) bool, char
  - fp-type: primitive rational type impled as floating point
    try make IEEE754-2008 more simple and understandable
  - string-type: primitive string type or compiler aware std string type
    utf-8, random byte access, sequential char access
  - generic dynamic array type
  - operator: basic arithmetic, logical, equality, compare operators
  - struct: struct like type definition, at least tuple
  - obp: at least object based, 
    type member field access, type member function call
  - fn-def: function definition, with parameter and optional return value

### Issue1: namespace, module and access level

see [file-level-physical-arch](file-level-physical-arch.md)

### Issue2: Attribute

see [Attribute](attribute.md)

