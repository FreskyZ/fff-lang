# fff-lang Designment Document

## Block lable

currently (0.1.2) fff-lang supports loop name specifier at loop statement:

    loop "loop-name" {
        ...
        for ... {
            break "loop-name";
        }
    }

It is not added to for and while statement, which makes it inconsistent

loop statement name specifier comes from rust's designment, which specify  
its block by lifetime specifier, which is part of its very complex lifetime  
concept. rust uses its lifetime syntax `' IDENT`, I do not want it because  
it uses character literal syntax and makes my v1lexer too complex

now I decided to add a lable syntax to not only 3 loop statements but also  
normal blocks:

    LABLE while ... { ... }
    LABLE for ... { ... }
    LABLE loop { ... }
    LABLE { ... }  // but free goto is not supported currently

so decided to use `@` for lables

    Lable: '@' ('@' | '_' | ALPHABETICAL_CHAR | NUMBER_CHAR)*

also '@' char contains some meaning of address, makes it more natural

in future there maybe also rust's also future designment:

    break|continue LABLE EXPR;

In conclution, add lable to lexical parser and 3 loop statements parser
