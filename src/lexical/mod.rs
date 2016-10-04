
mod symbol_type;
mod buf_lexer;
mod v0;
mod v1;
mod v2;
mod v3;

pub use self::v3::V3Token as Token;
pub use self::v3::BufV3Token as BufToken;
pub use self::v3::BufV3Lexer as Lexer;

pub use self::symbol_type::SeperatorKind;
pub use self::symbol_type::KeywordKind;
pub use self::symbol_type::CharLiteral;
pub use self::symbol_type::StringLiteral;
pub use self::symbol_type::NumericLiteral;

// TODOs
// numeric literal parser
// char parser optimize