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

prog:
  | query { Cst.Query $1 }
  ;

query:
  | term { Cst.Term {term= $1 } }
  ;

term: 
  | PERIOD { Cst.Identity }
  | RECURSE { Cst.Recurse }
  | NULL { Cst.Null }
  | FALSE { Cst.False }
  | TRUE { Cst.True }
  | INDEX { Cst.Index $1 }
  ;
