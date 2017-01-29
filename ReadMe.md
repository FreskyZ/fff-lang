# fff-lang

## Fresky's Favorite Feature Language

A small compiler that implements Fresky's favorite feature

Mainly include:

   - C style lexical rule, numeric literal, string literal,  
     [_a-zA-Z][_a-zA-Z0-9]* identifier, +-*\%^!~|?<>= operator,  
     ()[]{} seperator
   - Modern numeric literal, include different radius and width postfix
   - Modern string literal, raw string literal(looking at java),  
     string interpolation like C# and python(unimplemented)
   - C style basic syntax rule, expression, assignment, ifelse, while  
     statement
   - Haskell style array literal and tuple literal
   - C++ style range for
   - C++ style lambda expression
   - Loop statement for infinite loop and modern loop name specifier
   - Many kind of different width integral type and floating type, bool  
     type, char type, prevention of integral type and bool type conversion  
     prevention of assignment expression value usage
   - Built from source string to executable binary(currently only PE64)  
     AST generation, intermidate language generation, native code generation  
     lib format, external lib linkage, optimization all included
   - Bootstrap (unimplemented)
   - (Maybe) interpret and compile execution both supported
   - (Maybe) functional language style pattern matching supported
   - (Maybe) functional language style type class supported
   - (Maybe) functional language style function object supported
