
[x] make it pass compile from source change
[x] make syntax parser pass compile part 1
[x] upgrade keyword and separator generation script, fix THE TYPO
[x] change public token to new
[x] make syntax parser pass compile part 2
[x] migrate v2 token to new token
[x] migrate numeric literal parser to new token, move the comparator into test only
[x] upgrade is_identifier_start, is_identifier_continue, check_non_ascii_char by generating unicode tables by myself
[ ] merge v1 and v2 lexer, change intrusive BufLexer to syntax parser parse context style, call the end of age of 4 layers of lexer
[ ] try update lexical inteface to iterator style
[ ] remove all the Eq from cfg_attr(test)
[ ] make syntax parse pass compile