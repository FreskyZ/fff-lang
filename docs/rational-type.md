# fff-lang Designment Document

# rational primitive numeric type

Rename current floating point type to rational type

Because currently the name of floating point actually means 
'numeric value represented in floating point format', which do not follow the    
naming convention of integral type where the name is its feature not its  
implementation.

So rename them to rational type to indicate that they are rational numbers  
which can be represented as a division of 2 integers. Although their  
implementation represent them in floating point format, but this is OK,  
because something like u8(8 bit unsigned integer type) are actually  
implemented in 64 bit integer format, how they are implemented do not  
influence how they provide their meaning and feature.

Also, this brings new primitive type name `r32` and `r64` to replace previous  
`f32` and `f64` name and this replacement also happens in numeric literal.  
This also solve a case `0x123f32`, which is a valid bits type in hexadecimal  
format but looks like a hexadecimal floating point type, now it will change  
to `0x123r32`, which will correctly throw an error that hexadecimal bits type  
should not have rational postfix

Tracking issue:
   - Rename primitive type keyword `f32` and `f64` to `r32` and `r64`  
   - Change lexer/v2/num\_lit\_parser to adapt `r32` and `r64` postfix  
     make pass test
   - Change v2 error strings and rename them to rational type
   - Change codegen/TypeDefCollection's primitive types to rationals