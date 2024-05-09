/* symbols */
%token PIPE PERIOD SEMI COMMA COLON EQUAL DOLLAR
/* brackets */
%token LBRACKET RBRACKET LPAREN RPAREN LCURLY RCURLY
/* arithmetic op */
%token PLUS MINUS MUL DIV
/* logical operators */
%token AND OR NOT
/* constants */
%token NIL
%token <int> NUMBER_CONSTANT
%token <string> STRING_CONSTANT
%token TRUE FALSE
/* keywords */ 
%token DEF REDUCE FOREACH
/* EOF */
%token EOF

%start <Cst.program option> prog

%%

prog:
  | PERIOD { Some (Period ".") }
  | EOF { None };
