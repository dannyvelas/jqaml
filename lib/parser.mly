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
%type <Cst.query> query
%type <Cst.query> body

%start <Cst.query> prog

%%

prog:
  | body { $1 }
  ;

body:
  | query EOF { $1 }
  | EOF { Cst.Empty }
  ;

query:
  | expr { Cst.Expr $1 }
  | expr PIPE query { Cst.JoinedQuery ($1, $3) }
  ;

expr:
  | term { Cst.Term $1 }
  | expr PLUS term { Cst.ExprArithmetic ($1, Cst.Addition, $3) }
  | expr MINUS term { Cst.ExprArithmetic ($1, Cst.Subtraction, $3) }

term:
  | factor { Cst.Factor $1 }
  | term MUL factor { Cst.TermArithmetic ($1, Cst.Multiplication, $3) }
  | term DIV factor { Cst.TermArithmetic ($1, Cst.Division, $3) }

factor:
  | PERIOD { Cst.Identity }
  | literal { Literal $1 }

literal:
  | NULL { Cst.Null }
  | FALSE { Cst.False }
  | TRUE { Cst.True }
  | NUMBER_CONSTANT { Cst.Number $1 }
  ;

