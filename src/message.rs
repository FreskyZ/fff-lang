
// Process messages

use std::fmt;
use common::Position;
use common::StringPosition;
use common::format_vector_debug;

// Lexical
#[derive(Eq, PartialEq)]
pub enum LexicalMessage {

    UnexpectedEndofFileInBlockComment { 
        block_start: Position, 
        eof_pos: Position 
    },
    UnexpectedEndofFileInStringLiteral { 
        literal_start: Position, 
        eof_pos: Position, 
        hint_escaped_quote_pos: Option<Position> 
    },
    UnexpectedEndofFileInCharLiteral {
        literal_start: Position,
        eof_pos: Position,
    },
    UnrecognizedEscapeCharInStringLiteral {
        literal_start: Position,
        unrecogonize_pos: Position,
        unrecogonize_escape: char,
    },
    UnrecognizedEscapeCharInCharLiteral {
        literal_start: Position,
        unrecogonize_pos: Position,
        unrecogonize_escape: char,
    },

    UnexpectedUnicodeChar {
        ch: char, 
        pos: Position,
        unicode_name: String,
        ascii_ch: char,
        ascii_name: String,
    },

    UnexpectedIdentifierCharInNumericLiteral {
        literal_start: Position,
        unexpected_char: char,
    },
    UnexpectedCharInUnicodeCharEscape {
        escape_start: Position,
        unexpected_char_pos: Position,
        unexpected_char: char,        
    },
    IncorrectUnicodeCharEscapeValue {
        escape_start: Position,
        raw_value: String, 
    },
    
    UnexpectedStringLiteralEndInUnicodeCharEscape {
        literal_start: Position, 
        escape_start: Position, 
        unexpected_end_pos: Position,
    },
    UnexpectedCharLiteralEndInUnicodeCharEscape {
        literal_start: Position, 
        escape_start: Position, 
        unexpected_end_pos: Position,
    },

    // Numeric literal
    InvalidPrefixInNumericLiteral {
        literal_pos: StringPosition,
        prefix: char
    },
    EmptyNumericLiteral {
        literal_pos: StringPosition,
    },
    UnexpectedCharInNumericLiteral {
        literal_pos: StringPosition,
    },
    UnexpectedDecimalPointInIntegralLiteral {
        literal_pos: StringPosition,
    },
    UnexpectedMultiDecimalPointInFloatLiteral {
        literal_pos: StringPosition,
    },
    PrefixNotSupportedForFloatLiteral {
        literal_pos: StringPosition,
    },
    NumericLiteralTooLarge {
        literal_pos: StringPosition,
    },
    MultiSeperatorInNumericLiteral {
        literal_pos: StringPosition,
    },

    EmptyCharLiteral {
        pos: Position,
    },
    CharLiteralTooLong {
        start_pos: Position,
    },
    InvalidEscapeInCharLiteral {    // Special for '\'
        start_pos: Position,
    },
}

impl fmt::Debug for LexicalMessage {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::LexicalMessage::*;
        match *self {
            UnexpectedEndofFileInBlockComment { ref block_start, ref eof_pos } => {
                write!(f, "Unexpected end of file at {} in block comment starts from {}", eof_pos, block_start)
            }
            UnexpectedEndofFileInStringLiteral { ref literal_start, ref eof_pos, ref hint_escaped_quote_pos } => {
                try!(write!(f, "Unexpected end of file at {} in string literal starts from {}", eof_pos, literal_start));
                match hint_escaped_quote_pos {
                    &None => Ok(()),
                    &Some(ref hint) => write!(f, ", did you accidentally escape the quotation mark at {}?", hint),
                }
            }
            UnexpectedEndofFileInCharLiteral { ref literal_start, ref eof_pos } => {
                write!(f, "Unexpected end of file at {} in char literal starts from {}", eof_pos, literal_start)
            }
            UnrecognizedEscapeCharInStringLiteral { ref literal_start, ref unrecogonize_pos, ref unrecogonize_escape } => {
                write!(f, "Unrecogonized escape char {:?} at {} in string literal starts from {}", unrecogonize_escape, unrecogonize_pos, literal_start)
            }
            UnrecognizedEscapeCharInCharLiteral { ref literal_start, ref unrecogonize_pos, ref unrecogonize_escape } => {
                write!(f, "Unrecogonized escape char {:?} at {} in char literal starts from {}", unrecogonize_escape, unrecogonize_pos, literal_start)
            }
            UnexpectedIdentifierCharInNumericLiteral { ref literal_start, ref unexpected_char } => {
                write!(f, "Unexpected character {:?} in numeric litral starts from {}", unexpected_char, literal_start)
            }
            UnexpectedCharInUnicodeCharEscape { ref escape_start, ref unexpected_char_pos, ref unexpected_char } => {
                write!(f, "Unexpected char {:?} at {:?} in unicode char escape start from {:?}, unicode char escape is like \\uxxxx or \\Uxxxxxxxx", 
                    unexpected_char, unexpected_char_pos, escape_start)
            }
            IncorrectUnicodeCharEscapeValue { ref escape_start, ref raw_value } => {
                write!(f, "Incorrect unicode char escape value starts from {:?}, 0x{} is not a valid unicode code point", escape_start, raw_value)
            }
            MultiSeperatorInNumericLiteral { ref literal_pos } => {
                write!(f, "Multi seperator in numeric literal at {:?}, you can only use one of `'` or `_` at one numeric literal", literal_pos)
            }

            UnexpectedStringLiteralEndInUnicodeCharEscape { ref literal_start, ref escape_start, ref unexpected_end_pos } => {
                write!(f, "Unexpected end of string literal at {:?} in unicode char escape from {:?} in string literal starts from {:?}", 
                    unexpected_end_pos, escape_start, literal_start)
            }
            UnexpectedCharLiteralEndInUnicodeCharEscape { ref literal_start, ref escape_start, ref unexpected_end_pos } => {
                write!(f, "Unexpected end of char literal at {:?} in unicode char escape from {:?} in string literal starts from {:?}", 
                    unexpected_end_pos, escape_start, literal_start)
            }
            
            EmptyCharLiteral{ ref pos } => {
                write!(f, "Empty char literal at {:?}", pos)
            }
            CharLiteralTooLong{ ref start_pos } => {
                write!(f, "Char literal at {:?} too long", start_pos)
            }
            InvalidEscapeInCharLiteral{ ref start_pos } => {
                write!(f, "Invalid escape in char literal at {:?}, did you mean `'\\''` or `'\\\\'`?", start_pos)
            }

            InvalidPrefixInNumericLiteral { ref literal_pos, ref prefix } => {
                write!(f, "Invalid prefix '0{}' in numeric literal at {:?}", prefix, literal_pos)
            },
            EmptyNumericLiteral { ref literal_pos } => {
                write!(f, "Empty numeric literal at {:?}", literal_pos)
            }
            UnexpectedCharInNumericLiteral { ref literal_pos } => {
                write!(f, "Unexpected char in numeric literal at {:?}", literal_pos)
            }
            UnexpectedDecimalPointInIntegralLiteral { ref literal_pos } => {
                write!(f, "Unexpected decimal point in integral literal at {:?}", literal_pos)
            }
            UnexpectedMultiDecimalPointInFloatLiteral { ref literal_pos } => {
                write!(f, "Unexpected multi decimal point in float literal at {:?}", literal_pos)
            }
            PrefixNotSupportedForFloatLiteral { ref literal_pos } => {
                write!(f, "Prefix not supported for float literal at {:?}", literal_pos)
            }
            NumericLiteralTooLarge { ref literal_pos } => {
                write!(f, "Numeric literal too large at {:?}", literal_pos)
            }

            UnexpectedUnicodeChar { ref ch, ref pos, ref unicode_name, ref ascii_ch, ref ascii_name } => {
                write!(f, "Unexpected unicode char {:?}({}) @ {:?}, do you mean ascii char {:?}({})?", 
                    ch, unicode_name, pos, ascii_ch, ascii_name
                )
            }
        }
    }
}
impl_display_by_debug!{ LexicalMessage }

#[derive(Eq, PartialEq)]
pub enum SyntaxMessage {

    // Syntax!
    ExpectSymbol{ expect: String, actual: String, pos: Position },

    // First recoverable of syntax!!!
    EmptySubscription{ pos: StringPosition },
    SingleCommaInNonArgumentFunctionDef{ fn_pos: StringPosition, comma_pos: Position },
    SingleCommaInFunctionCall{ call_pos: StringPosition, comma_pos: Position },
    SingleCommaInSubscription{ sub_pos: StringPosition, comma_pos: Position },

    SingleItemTupleType{ pos: StringPosition },

    LoopNameSpecifierIsNotStringLiteral{ pos: StringPosition },  // pos for the expression
    
    NotFunctionCallIndependentExpressionStatement { pos: StringPosition }, // pos for the statement
}

impl fmt::Debug for SyntaxMessage {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::SyntaxMessage::*;
        match *self {
            ExpectSymbol{ ref expect, ref actual, ref pos } => {
                write!(f, "Expect {} at {:?} but actually is {:?}", expect, pos, actual)
            }
            EmptySubscription{ ref pos } => {
                write!(f, "Empty subscription at {:?}", pos)
            }
            SingleCommaInNonArgumentFunctionDef{ ref fn_pos, ref comma_pos } => {
                write!(f, "Single comma at {:?} in no argument function def at {:?}", comma_pos, fn_pos)
            }
            SingleCommaInFunctionCall{ ref call_pos, ref comma_pos } => {
                write!(f, "Single comma at {:?} in function call at {:?}", comma_pos, call_pos)
            }
            SingleCommaInSubscription{ ref sub_pos, ref comma_pos } => {
                write!(f, "Single comma at {:?} in subscription at {:?}", comma_pos, sub_pos)
            }
            SingleItemTupleType{ ref pos } => {
                write!(f, "Single element tuple type at {:?}", pos)
            }
            LoopNameSpecifierIsNotStringLiteral{ ref pos } => {
                write!(f, "Loop name specifier is not string literal at {:?}", pos)
            }
            NotFunctionCallIndependentExpressionStatement{ ref pos } => {
                write!(f, "Invalid expression statement at {:?}, only function call or assignment can be expression statement", pos)
            }
        }
    }
}
impl_display_by_debug!{ SyntaxMessage }

impl SyntaxMessage {
    pub fn is_recoverable(&self) -> bool {
        match *self {
            SyntaxMessage::ExpectSymbol{ .. } => false,
            SyntaxMessage::EmptySubscription{ .. } => true,
            SyntaxMessage::SingleCommaInNonArgumentFunctionDef{ .. } => true,
            SyntaxMessage::SingleCommaInFunctionCall{ .. } => true,
            SyntaxMessage::SingleCommaInSubscription{ .. } => true,
            SyntaxMessage::SingleItemTupleType{ .. } => true,
            SyntaxMessage::LoopNameSpecifierIsNotStringLiteral{ .. } => true,
            SyntaxMessage::NotFunctionCallIndependentExpressionStatement{ .. } => true,
        }
    }
}

#[derive(Eq, PartialEq)]
pub enum CodegenMessage {
    FunctionHasSameName{ 
        name: String, 
        poss: Vec<StringPosition>,
    },
    TypeNotExist{ 
        name: String, 
        pos: StringPosition 
    },
    FunctionArgumentNameConfilict{ 
        func_pos: StringPosition,  // position of the fn 
        func_name: String,        
        name: String,          
        pos1: StringPosition,      // first def of the name
        pos2: StringPosition,      // more def of the name
    },
    VariableDefined{
        name: String,
        predef: StringPosition,    // Previous VarDeclStmt
        curdef: StringPosition,    // Current VarDeclStmt, compiler generated will not collision
    },
    FunctionRedefinition{
        sign: String,
        fnposs: Vec<StringPosition>,
    },

    ConstVariableDeclareMissingInitializationExpression{
        name: String,
        pos: StringPosition,
    },

    FunctionCallOperatorNotAppliedToIdentifier{
        pos: StringPosition,
    },

    InvalidExpressionStatementSingleSimpleExpression{
        pos: StringPosition,
    },
    InvalidExpressionStatementLastOpNotValid{
        pos: StringPosition,
    },
    LeftOfAssignmentStatementCannotBeComplex{
        pos: StringPosition,
    },
    AssignToConstVar{
        name: String,
        pos: StringPosition, // stmt pos
    },
    MemberAccessNotSupportedCurrently{
        pos: StringPosition,
    },

    CannotFindLoopName{
        name: String, 
        pos: StringPosition,
    },
    JumpStatementNotInLoop{
        pos: StringPosition
    },

    ArrayInitializeElementNotSameType{
        expect: String,
        actual: String,
        pos: StringPosition,
    },
    ArrayDupDefSecondParameterTypeNotExpected{
        actual: String,
        pos: StringPosition,
    },

    MemberNotExist{
        prev_expr_pos: StringPosition, // previous part position
        prev_type_desc: String,        // previous part type display name
        op_pos: StringPosition,        // this op pos
        member_name: String,           // member name
    },

    IdentNotDeclared{
        name: String,
        pos: StringPosition,
    },
    FunctionNotDefined{
        sign: String,
        pos: StringPosition,
    },
    AssignmentTypeMismatch{
        left_desc: String,
        right_desc: String,
        pos: StringPosition,
    },
    IfConditionNotBoolType{
        pos: StringPosition,
        actual: String,
    },
    ReturnTypeMismatch{
        pos: StringPosition,
        expect: String,
        actual: String,
    },
    ForIteraterTypeMismatch{
        pos: StringPosition,
        actual: String,
    },
    ForRangeTypeMismatch{
        pos: StringPosition,
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
                write!(f, "**CURRENTLY** if iterater only supports integral type, meet {} at {:}", actual, pos)
            },
            ForRangeTypeMismatch{ ref pos, ref actual, ref expect } => {
                write!(f, "For range type mismatch at {:?}, expect {}, actual is {}", pos, expect, actual)
            },
        }
    }
}
impl_display_by_debug!{ CodegenMessage }

#[derive(Eq, PartialEq)]
pub enum RuntimeMessage {
    CannotFindMain,
    ConvertNonBoolToBool,
}
impl fmt::Debug for RuntimeMessage {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            RuntimeMessage::CannotFindMain => {
                write!(f, "Can not find a method with name `main` and no argument and no return type")
            }
            RuntimeMessage::ConvertNonBoolToBool => {
                write!(f, "Try convert non bool type to bool type")
            }
        }
    }
}
impl_display_by_debug!{ RuntimeMessage }

#[derive(Eq, PartialEq)]
pub enum Message {
    Lexical(LexicalMessage),
    Syntax(SyntaxMessage),
    Codegen(CodegenMessage),
    Runtime(RuntimeMessage),
}

impl From<LexicalMessage> for Message {
    fn from(msg: LexicalMessage) -> Message { Message::Lexical(msg) }   
}
impl From<SyntaxMessage> for Message {
    fn from(msg: SyntaxMessage) -> Message { Message::Syntax(msg) }   
}
impl From<CodegenMessage> for Message {
    fn from(msg: CodegenMessage) -> Message { Message::Codegen(msg) }   
}
impl From<RuntimeMessage> for Message {
    fn from(msg: RuntimeMessage) -> Message { Message::Runtime(msg) }
}

impl fmt::Debug for Message {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Message::Lexical(ref msg) => write!(f, "{:?}", msg),
            Message::Syntax(ref msg) => write!(f, "{:?}", msg),
            Message::Codegen(ref msg) => write!(f, "{:?}", msg),
            Message::Runtime(ref msg) => write!(f, "{:?}", msg),
        }
    }
}
impl_display_by_debug!{ Message }

#[cfg(test)]
#[derive(Eq, PartialEq)]
pub struct MessageEmitter {
    messages: Vec<Message>,
}
#[cfg(not(test))]
pub struct MessageEmitter {
    messages: Vec<Message>,
}
impl fmt::Debug for MessageEmitter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for message in &self.messages {
            try!(writeln!(f, "{:?}", message));
        }
        Ok(())
    }
}
impl_display_by_debug!(MessageEmitter);
impl MessageEmitter {

    pub fn new() -> MessageEmitter {
        MessageEmitter { messages: Vec::new() }
    }

    pub fn is_empty(&self) -> bool { self.messages.is_empty() }

    pub fn push<T: Into<Message>>(&mut self, message: T) {
        self.messages.push(message.into());
    }

    pub fn pop(&mut self) { // Pop top
        let _ = self.messages.pop();
    }
}
