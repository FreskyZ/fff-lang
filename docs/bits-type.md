# fff-lang Designment Document

## bits8, bits16, bits32, bits64 type

When I'm implementing binary expression parser in syntax parser, in a  
random generated test, I found this slice:

    1 + 2 >> 3

according to the original syntax definition partially copied from C++,shift   
operators priority is lower then additive operators, so the expression is  
parsed like this:

    (1 + 2) >> 3

which I think is strange: shift operators are sugar of multiplicative  
operators, why they are lower than additive?

I found this on stackoverflow: 

[Curiosity: why the shift operators have less priority than the additive?](http://stackoverflow.com/questions/7844756/curiosity-why-the-shift-operators-have-less-priority-than-the-additive/7845322#7845322)

the first answer explains why, **numeric type is not bit array type**,  
this also reminds of my another thinking: I don't want to be like C#'s enum  
type, my enum type and bitflags type should not be same, enum type should  
be a sugar for const integral static fields, whose value only for  
identification and cannot cast from or to integral types, only equality  
operators can apply on them, bitflags type should also prevent integral value  
conversion, but also provide bitwise operators on them

Combine the 2 thinkings, I designed the bits** type

   - 4 primitive type named `bits8`, `bits16`, `bits32` and `bits64`
   - add them to keywords list
   - literal must have `0b`, `0o` or `0x` prefix, no prefix is not allowed,  
    because no prefix is regarded as decimal literal, where decimal literal  
    bits cannot match exactly with binary bits, causes low readability
   - literal can have `bits8`, `bits16`, `bits32` and `bits64` postfixes,  
    if not provided, like `0b10100101`, is regarded as `bits32` type
   - auto expansion provided, `0xABCD1234ABCD` is parsed as `bits64` type
   - bits type have those operator, in priority order: 
     1. shift operators
     2. bitand operator
     3. bitxor operator
     4. bitor operator
     5. eqaulity operator  
     no multiplicative, additive, relational operator provided
   - in additional, bitwise operators, shift operators are removed from  
    integral types, now the available operators for integral type, in order: 
     1. multiplicative operators
     2. additive operators
     3. relational operators
     4. equality operators
    - in additional, bool type still have these operators, in order:
     1. eqaulity operators
     2. logicaland operator
     3. logicalor operator

In future version of this language, when type definition is added, an  
enum type will be provided, where you can use any of the integral types  
as base type, unsigned and signed version of same width integral type  
is irrelevant, because they only provide width, where 64bits by default

Similar but not same, an `extended bits` type will be provided, which have  
similar syntax rule as enum types, but you provide bits type as base type,  
it is a stronger type version of bits type, where you can use bits to  
define the field values, use bitwise operators on fields and literal values,  
but after definition, you cannot apply any operators between extended bits type  
and its underlaying bits type

By the way, the bits type are not named in `b8`, `b32` because they are rare  
to use, I don't want them to occupy too much of the keyword namespace and  
identifier namespace