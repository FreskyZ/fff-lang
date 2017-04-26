# fff-lang Designment Document

## Increment and decrement operators

They are removed in 0.1.2

In concern of code human readability, increment and decrement unary  
expression, as an assignment statement, do not have a visible equation char  
which maybe confusing, and the very simple `++a;` statment looks more ugly  
then `a += 1;`.

And for compiler implementation, these 2 operators add complexity to  
code gener where I have to check more conditions to allow it to be a single  
statement.

Currently it is used in for statement, where an increment operator call is  
applied on the iterated variable, it is not a good idea because not all 
iterable object can be random accessed, so will change it to something like  
`get_iter` and `next`, detailed designment to be considered.

So, no increment and decrement operators are removed