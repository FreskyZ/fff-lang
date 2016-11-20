
// generate vm code from AST

// vm is a stack based virtual machine
// expand each expression-statement to vm codes, every expression-statement is independent
// other statements are all flow control statements, just add instructions include Goto, GotoIfTrue and GotoIfFalse
// expressions are expanded according to ExpressionOperators
//     special occassions, array.set_Index is combined from operator[] and operator=
// ExpressionOperators perform actions on evaluate stack, pop items and push items, 
// load local and store local interact between call stack and eval stack

// call stack and eval stack for functions are independent, so that rbp and rsp are not used
// vm will maintain the local variables with name as key
// a special vmcode called scope enter and scope leave is emitted at block enter and block leave, for block local variables

// In detail, expansion is 
// first, combine operator index and operator assignment, include op assignment to special set index and op set index
// only has last expr op and last expr op is function call or set index or op set index is allowed in expr statement
// then, make every expression in following ops to be pure base expression
// that is, move the operators and expr base to another statement and store the result in a generated local, and replace
//     previous expression by this temp local

// First optimization!
// Remove pair of LoadLocal and StoreLocal which have same name

mod vm_code;
mod session;
mod expr_stmt;
mod type_def;
mod function_def;
mod block;
mod variable;

pub use codegen::type_def::TypeID;
pub use codegen::type_def::TypeDecl;
pub use codegen::type_def::TypeDeclCollection;
pub use codegen::function_def::FnID;
pub use codegen::function_def::FnArg;
pub use codegen::function_def::FnImpl;
pub use codegen::function_def::FnCollection;
pub use codegen::block::Block;
pub use codegen::variable::VarID;
pub use codegen::variable::Var;
pub use codegen::variable::VarOrScope;
pub use codegen::variable::VarCollection;
pub use codegen::vm_code::VMCode;
pub use codegen::vm_code::VMCodeCollection;
pub use codegen::session::Program;

use syntax::Program as SyntaxProgram;
use codegen::session::GenerationSession;

pub fn generate(program: SyntaxProgram) -> Program {
    GenerationSession::dispatch(program)
}