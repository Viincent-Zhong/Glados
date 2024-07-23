```
<program> ::= <statement> | <statement> <program>

<statement> ::= <import> | <assignment> | <expression> | <comment> | <function>

<import> ::= "import" <filename>

<filename> ::= <string>

<string> ::= '"' <letter>* '"'

<letter> ::= "a" | "b" | "c" | ... | "z" | "A" | "B" | "C" | ... | "Z"

<assignment> ::= <identifier> "=" <expression> | <identifier> "define" <expression>

<expression> ::= <infix-expression> | <identifier> | <number> | <function-call> | <lambda>

<infix-expression> ::= "(" <expression> <operator> <expression> ")"

<operator> ::= "+" | "add" | "-" | "sub" | "*" | "mul" | "/" | "div" | "%" | "mod" | "eq?" | '<'

<identifier> ::= <string>

<number> ::= <integer>

<integer> ::= <digit>+

<digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

<comment> ::= "#" <char>* "\n"

<lambda> ::= "lambda" "(" <arg-list> ")" <expression>

<function> ::= "(( " <identifier> <arg-list> ")" "=" <expression> ")"

<arg-list> ::= <identifier>*

<function-call> ::= "(" <function-name> <expression>* ")"

<function-name> ::= <string>

```
