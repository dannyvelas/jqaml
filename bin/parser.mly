/* symbols */
%token PIPE PERIOD SEMI COMMA COLON EQUAL DOLLAR RECURSE INDEX
/* brackets */
%token LBRACKET RBRACKET LPAREN RPAREN LCURLY RCURLY
/* arithmetic op */
%token PLUS MINUS MUL DIV
/* logical operators */
%token AND OR NOT
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
  | term { Cst.Term {term= Cst.Identity } }
  ;

term: 
  | PERIOD { Cst.Identity }
  | RECURSE { Cst.Recurse }
  | NULL { Cst.Null }
  | FALSE { Cst.False }
  | TRUE { Cst.True }
  ;
