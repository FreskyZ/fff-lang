///! legacy message definitions
///! leave here because want to completely update messages and this crate is not working currently

#[derive(Eq, PartialEq)]
pub enum CodegenMessage {
    FunctionHasSameName{ 
        name: String, 
        poss: Vec<Span>,
    },
    TypeNotExist{ 
        name: String, 
        pos: Span 
    },
    FunctionArgumentNameConfilict{ 
        func_pos: Span,  // position of the fn 
        func_name: String,        
        name: String,          
        pos1: Span,      // first def of the name
        pos2: Span,      // more def of the name
    },
    VariableDefined{
        name: String,
        predef: Span,    // Previous VarDeclStmt
        curdef: Span,    // Current VarDeclStmt, compiler generated will not collision
    },
    FunctionRedefinition{
        sign: String,
        fnposs: Vec<Span>,
    },

    ConstVariableDeclareMissingInitializationExpression{
        name: String,
        pos: Span,
    },

    FunctionCallOperatorNotAppliedToIdentifier{
        pos: Span,
    },

    InvalidExpressionStatementSingleSimpleExpression{
        pos: Span,
    },
    InvalidExpressionStatementLastOpNotValid{
        pos: Span,
    },
    LeftOfAssignmentStatementCannotBeComplex{
        pos: Span,
    },
    AssignToConstVar{
        name: String,
        pos: Span, // stmt pos
    },
    MemberAccessNotSupportedCurrently{
        pos: Span,
    },

    CannotFindLoopName{
        name: String, 
        pos: Span,
    },
    JumpStatementNotInLoop{
        pos: Span
    },

    ArrayInitializeElementNotSameType{
        expect: String,
        actual: String,
        pos: Span,
    },
    ArrayDupDefSecondParameterTypeNotExpected{
        actual: String,
        pos: Span,
    },

    MemberNotExist{
        prev_expr_pos: Span, // previous part position
        prev_type_desc: String,        // previous part type display name
        op_pos: Span,        // this op pos
        member_name: String,           // member name
    },

    IdentNotDeclared{
        name: String,
        pos: Span,
    },
    FunctionNotDefined{
        sign: String,
        pos: Span,
    },
    AssignmentTypeMismatch{
        left_desc: String,
        right_desc: String,
        pos: Span,
    },
    IfConditionNotBoolType{
        pos: Span,
        actual: String,
    },
    ReturnTypeMismatch{
        pos: Span,
        expect: String,
        actual: String,
    },
    ForIteraterTypeMismatch{
        pos: Span,
        actual: String,
    },
    ForRangeTypeMismatch{
        pos: Span,
        expect: String,
        actual: String,
    }
}
impl fmt::Debug for CodegenMessage {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::CodegenMessage::*;
        match *self {
            FunctionHasSameName{ ref name, ref poss } => {
                write!(f, "Function with name `{}` are defined mutliple time at {}", name, format_vector_debug(poss, ", "))
            },
            TypeNotExist{ ref name, ref pos } => {
                write!(f, "Type {} at {:?} not defined or not in scope", name, pos)
            },
            FunctionArgumentNameConfilict{ ref func_pos, ref func_name, ref name, ref pos1, ref pos2 } => {
                write!(f, "Parameter {} redifinition at {:?}, whose previous def at {:?} in function {} at {:?}",
                    name, pos2, pos1, func_name, func_pos
                )
            },
            VariableDefined{ ref name, ref predef, ref curdef } => {
                write!(f, "Variable {} already defined at {:?} but redefiened at {:?}", name, predef, curdef)
            },
            FunctionRedefinition{ ref sign, ref fnposs } => {
                write!(f, "Function with signature `{}` redefined at {}", sign, format_vector_debug(fnposs, ", "))
            },
            ConstVariableDeclareMissingInitializationExpression{ ref name, ref pos } => {
                write!(f, "Variable declaration of {} at {:?} is const but missing init expression", name, pos)
            },
            FunctionCallOperatorNotAppliedToIdentifier{ ref pos } => {
                write!(f, "Function call operator not applied to identifier at {:?}", pos)
            },
            InvalidExpressionStatementSingleSimpleExpression{ ref pos } => {
                write!(f, "Invalid expression statement at {:?}, simple expression cannot be statement", pos)
            },
            InvalidExpressionStatementLastOpNotValid{ ref pos } => {
                write!(f, "Invalid expression statement at {:?}, expression statement should be at last a function call statement or increase or decrease expression", pos)
            },
            LeftOfAssignmentStatementCannotBeComplex{ ref pos } => {
                write!(f, "Left expression at {:?} of assignment statement can only be an identifier", pos)
            },
            AssignToConstVar{ ref name, ref pos } => {
                write!(f, "Try to assign to const variable {:?} at {:?}", name, pos)
            },
            MemberAccessNotSupportedCurrently{ ref pos } => {
                write!(f, "Member access not supported **CURRENTLY** at {:?}", pos)
            },
            CannotFindLoopName{ ref name, ref pos } => {
                write!(f, "Cannot find loop name `{}` in jump statement at {:?}", name, pos)
            },
            JumpStatementNotInLoop{ ref pos } => {
                write!(f, "Jump statement at {:?} not in any loop", pos)
            },
            ArrayInitializeElementNotSameType{ ref expect, ref actual, ref pos } => {
                write!(f, "Unexpected type of initialize element in array at {:?}, expect {}, actual is {}", pos, expect, actual)
            },
            ArrayDupDefSecondParameterTypeNotExpected{ ref actual, ref pos } => {
                write!(f, "Second parameter of array dup def at {:?} type miss match, expect i32 or u64, acutal is {}", pos, actual)
            },
            MemberNotExist{ ref prev_expr_pos, ref prev_type_desc, ref op_pos, ref member_name } => {
                write!(f, "Member {} at {:?} not exist for expression at {:?} with type {}", member_name, op_pos, prev_expr_pos, prev_type_desc)
            },
            IdentNotDeclared{ ref name, ref pos } => {
                write!(f, "Identifier `{}` not defined at {:?}", name, pos)
            },

            FunctionNotDefined{ ref sign, ref pos } => {
                write!(f, "Function with signature {} not defined at scope of the expression at {:?}", sign, pos)
            },
            AssignmentTypeMismatch{ ref left_desc, ref right_desc, ref pos } => {
                write!(f, "Assignment type mismatch at {:?}, left is `{}`, right is `{}`", pos, left_desc, right_desc)
            },
            IfConditionNotBoolType{ ref pos, ref actual } => {
                write!(f, "Expect if condition to be bool type, but actually is {} at {:?}", actual, pos)
            },
            ReturnTypeMismatch{ ref pos, ref actual, ref expect } => {
                write!(f, "Return type mismatch at {:?}, expect {}, actual is {}", pos, expect, actual)
            },
            ForIteraterTypeMismatch{ ref pos, ref actual } => {
                write!(f, "**CURRENTLY** if iterater only supports integral type, meet {} at {:?}", actual, pos)
            },
            ForRangeTypeMismatch{ ref pos, ref actual, ref expect } => {
                write!(f, "For range type mismatch at {:?}, expect {}, actual is {}", pos, expect, actual)
            },
        }
    }
}