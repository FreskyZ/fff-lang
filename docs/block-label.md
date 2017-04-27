# fff-lang Designment Document

## Add block label

from v0.1.0 we support loop name specifier in loop statement:

    LoopStatement = fLoop Expression Block

where Expression must be a simple string literal, this feature supports some  
usage like 

    loop "1" {
        loop "loop2" {
            ...
            continue "loop2";
            ...
            break "1";
        }
    }

which is very sweet at some time

But, the feature is not added to for statement and while statement, because  
I don't know where to add the loop name specifier

    ForStatement = 
        fFor fIdentifier fIn Expression fRange Expression Block
        ^ --- cannot distinguish with expression
         2 expression seperated by space may cause 
         unexpected parse result and low readability --- ^

while statement is similiar, this makes this feature inconsistent, which is  
not natural and not natural

in rust, where the loop name specifier comes from, have (in my syntax syntax)

    ForStatement = 
        [LifeTime fColon] fFor fIdent [fColon TypeUse] fIn Expression Block
    WhileStatment =
        [LifeTime fColon] fWhile Expression Block
    LoopStatment = 
        [LifeTime fColon] fLoop Block

not include while let statement, not relevant here.  
It is based on rust's lifetime mechanism, and even if you don't specify it,  
rustc will automatically add a lifetime to every block if is needed, and  
rust has its own special lexer to distinguish character literal and lifetime  
specifier

but I don't want to change my lexer to support this rather strange kind of  
lifetime token, and currently I do not have that complex lifetime mechanism

in C style language there are assembly style labels:

    Identifier fColon Everything

where the identifier maybe confused with variables (not in assembly codes  
where every identifier is actually label to code seg or data seg)

gcc supports a very special label array language extension

    void* labels[2] = { &&Label1, &&Label2 }
    Label1: xxx
    Label2: yyy
    goto *labels[0];

I don't know what is that double get address going to do, but it generally is  
understandable, labels are pointers to code segment, they can be regarded as  
void* and be gotoed

Currently I won't support raw gotos include this label array, because they  
are raw, unsafe and difficult to do control flow analyze and optimization

so I decided to add a new label syntax, use a new special char for it, as  
underscore is reserved for future pattern matching, $ is reserved for future  
macro, # is reserved for future attribute, ~ is bit not, ` is normally for  
code quote in many places, then the only left character on standard keyboard  
is @

in detail, the label lexical token is defined as

    Label = @[_@a-zA-Z0-9]*

single @ is supported, double @@ (yes neta from masm) is supported, I donot  
like more @s like @@@ and @@@@@@@, but I won't forbid it like I didn't forbit  
single character identifiers, they are too strict and I won't write this  
style code and I'm lazy to add this kind of check into lexical parser

And changes to syntax design:

    LoopStatement = 
        [fLabel fColon] fLoop Block
    WhileStatement = 
        [fLabel fColon] fWhile Expression Block
    ForStatement =
        [fLabel fColon] fFor fIdent fIn Expression fRange Expression Block
    BlockStatement =
        [fLabel fColon] Block
    BreakStatement =
        fBreak [fLabel] fSemiColon
    ContinueStatement = 
        fContinue [fLabel] fSemiColon

And now break statement are also available in block statement, which acts  
like C style language's `do { ... } while(0)` pattern which usually lies  
in macro definition to support break statement in that block

    @@: {
        ...
        break @@;
    }

is same as 

    do {
        ...
        break;
    } while (0);

also, label arrays or some equavalent maybe introduced in future, which bring  
in another syntax item that have to be pre resolved like functions, which  
requires name resolve to be completely refactored and maybe move into syntax  
parse