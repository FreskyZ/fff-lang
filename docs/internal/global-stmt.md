# fff-lang Internal Document

## Global Statements

after commit fee9e1f4f7b5fbcfaef579b0155fd705bf1c4209, statements is allowed in global scope, which seems more cool and pythonic, and makes helloworld shorter and simpler

but when adding use and import statements, allow import in local scope seems strange and meaningless, also, break, continue and return statements should not be at global scope, so a new global statement enumeration
