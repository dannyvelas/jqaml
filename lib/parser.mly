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
%token EQ NEQ
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
%type <Cst.expr> term
%type <Cst.expr> factor
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
  | term { $1 }
  | expr PLUS term { Cst.Binary ($1, Cst.Addition, $3) }
  | expr MINUS term { Cst.Binary ($1, Cst.Subtraction, $3) }
  ;

term:
  | factor { $1 }
  | term MUL factor { Cst.Binary ($1, Cst.Multiplication, $3) }
  | term DIV factor { Cst.Binary ($1, Cst.Division, $3) }
  ;

factor:
  | PERIOD { Cst.Identity }
  | literal { Cst.Literal $1 }
  | LPAREN expr RPAREN { Cst.Grouping $2 }
  | MINUS factor { Cst.Unary $1 $2 }
  ;

literal:
  | NULL { Value.Null }
  | FALSE { Value.False }
  | TRUE { Value.True }
  | NUMBER_CONSTANT { Value.Number $1 }
  | STRING_CONSTANT { Value.String $1 }
  ;

