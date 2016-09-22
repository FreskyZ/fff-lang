
// Level2 parser
// input v1
// Hide spaces
// Output 
//      escaped string literal, \n\r\t\"\\
//      evaluated numeric literal, only i32, if not in [0-9] error
//      identifier or keyword
//      operators, +, -, *, /, %, +=, -=, *=, /=, %=, .
//      seperators, [, ], {, }, (, ), ;, ,
// May be final layer