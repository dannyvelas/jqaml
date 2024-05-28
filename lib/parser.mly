%token <string> IDENTIFIER
/* symbols */
%token ASSIGN PIPE PERIOD SEMI COMMA COLON EQUAL DOLLAR RECURSE
/* brackets */
%token LBRACKET RBRACKET LPAREN RPAREN LCURLY RCURLY
/* arithmetic op */
%token PLUS MINUS MUL DIV
/* logical operators */
%token AND OR NOT
/* comparison operators */
%token EQ NEQ GT LT GTE LTE
/* indexing */
%token <string> INDEX
/* constants */
%token NULL
%token <int> NUMBER_CONSTANT
%token <string> STRING_CONSTANT
%token TRUE FALSE
/* keywords */ 
%token DEF REDUCE FOREACH
/* delimiters */
%token EOL EOF

%start <Cst.program> prog

%type <Cst.query> query
%type <Cst.expr> expr
%type <Cst.expr> equality
%type <Cst.expr> comparison
%type <Cst.expr> term
%type <Cst.expr> factor
%type <Cst.expr> primary
%type <Value.value> literal

%%

prog:
  | query EOF { Cst.Query $1 }
  | EOF { Cst.Empty }
  ;

query:
  | expr { Cst.Expr $1 }
  | expr PIPE query { Cst.JoinedQuery ($1, $3) }
  ;

expr:
  | equality { $1 }
  ;

equality:
  | equality EQ comparison { Cst.Binary ($1, Cst.Equal, $3) }
  | equality NEQ comparison { Cst.Binary ($1, Cst.NotEqual, $3) }
  | comparison { $1 }
  ;

comparison:
  | comparison GT term { Cst.Binary ($1, Cst.GreaterThan, $3) }
  | comparison GTE term { Cst.Binary ($1, Cst.GreaterThanEqual, $3) }
  | comparison LT term { Cst.Binary ($1, Cst.LessThan, $3) }
  | comparison LTE term { Cst.Binary ($1, Cst.LessThanEqual, $3) }
  | term { $1 }
  ;

term:
  | term PLUS factor { Cst.Binary ($1, Cst.Addition, $3) }
  | term MINUS factor { Cst.Binary ($1, Cst.Subtraction, $3) }
  | factor { $1 }
  ;

factor:
  | factor MUL primary { Cst.Binary ($1, Cst.Multiplication, $3) }
  | factor DIV primary { Cst.Binary ($1, Cst.Division, $3) }
  | primary { $1 }
  ;

primary:
  | PERIOD { Cst.Identity }
  | literal { Cst.Literal $1 }
  | LPAREN query RPAREN { Cst.Grouping $2 }
  | PLUS primary { Cst.Unary (Cst.Positive, $2) }
  | MINUS primary { Cst.Unary (Cst.Negative, $2) }
  ;

literal:
  | NULL { Value.Null }
  | FALSE { Value.Bool false }
  | TRUE { Value.Bool true }
  | NUMBER_CONSTANT { Value.Number $1 }
  | STRING_CONSTANT { Value.String $1 }
  ;

