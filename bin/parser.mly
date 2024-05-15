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
/* EOF */
%token EOF

%start <Cst.program> prog

%%

prog: expr EOF { Cst.Expr $1 }

expr :
  | NUMBER_CONSTANT { Cst.Number $1 }
  | NUMBER_CONSTANT PLUS NUMBER_CONSTANT { Cst.Term ($1, Cst.PLUS, $3) } 
  ;
