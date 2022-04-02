///! fff-lang
///! 
///! syntax/jump_stmt for break statement and continue statement

#[cfg(test)] #[test]
fn jump_stmt_parse() {use super::prelude::*;
    
    case!{ "continue;" as ContinueStatement, 
        make_stmt!(continue 0:8)
    }
    case!{ "continue @1;" as ContinueStatement,
        make_stmt!(continue 0:11 label #2 9:10)
    }
    
    case!{ "break;" as BreakStatement,
        make_stmt!(break 0:5)
    }
    case!{ "break @1;" as BreakStatement,
        make_stmt!(break 0:8 label #2 6:7)
    }
}
