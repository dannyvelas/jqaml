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

%type <Cst.term> term
%type <Cst.query option> query

%start <Cst.program> prog

%%

/* TODO:
  for the byte input "<PERIOD> <EOF>"
  expection:
    new program: (Cst.Query (Cst.Term (Cst.Identity [])))
  reality
    new program: (Cst.Query (Cst.Term (Cst.Identity [])))
    new program: Cst.Empty
  why does the parser run twice?
  anser: because once the lexer hits EOF, it will continue emitting EOF every single time the parser calls it.
  the "EOF" is kind of like stuck in the chamber. so, it will the parser will call the lexer the first time and receive an EOF and create a parse tree,
  and then your main program will call the parser again and the parser will call the lexer again and receive an EOF again and create a parse tree again
*/
prog:
  | query { (Cst.Query $1) }
  ;

query:
| term EOL { Some (Cst.Term $1) }
| EOF { None }
;

term:
  | PERIOD { Cst.Identity [] }
  | RECURSE { Cst.Recurse }
  | NULL { Cst.Null }
  | FALSE { Cst.False }
  | TRUE { Cst.True }
  | INDEX { Cst.Index $1 }
  | NUMBER_CONSTANT { Cst.Number $1 }
  ;
