
1. make it pass compile from source change
2. flatten token type and remove a lot of test helpers
2?. make syntax parse pass compile
3. upgrade keyword/separator generation, merge .grammar file into token.py and move to scripts folder, fix separator's typo
4. try use syntax parser style parse context, merge major parsers and change literal parsers into "if match token start" from current "loop match current chars"