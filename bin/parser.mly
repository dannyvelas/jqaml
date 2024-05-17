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
%type <Cst.query option> body

%start <Cst.query option> prog

%%

prog:
  | body { $1 }
  ;

body:
  | query EOF { Some $1 }
  | EOF { None }
  ;

query:
  | term { Cst.Term $1 }
  /*| query PIPE query { Cst.JoinedQuery ($1, Cst.Pipe, $3) }*/
  ;

term:
  | PERIOD { Cst.Identity [] }
  | RECURSE { Cst.Recurse }
  | NULL { Cst.Null }
  | FALSE { Cst.False }
  | TRUE { Cst.True }
  | INDEX { Cst.Index $1 }
  | NUMBER_CONSTANT { Cst.Number $1 }
  | PERIOD suffix 
    {
      match $2 with
      | Cst.Iteration -> Cst.Identity [$2]
      | _ -> Cst.BracketSuffix $2
    }
  | PERIOD STRING_CONSTANT { Cst.Index ($2) }
  ;

suffix:
  | LBRACKET RBRACKET { Cst.Iteration }
  | LBRACKET NUMBER_CONSTANT RBRACKET { Cst.BracketIndex (Cst.BracketQuery $2) } 
  | LBRACKET NUMBER_CONSTANT COLON RBRACKET { Cst.BracketIndex (Cst.StartSlice $2) }
  | LBRACKET COLON NUMBER_CONSTANT RBRACKET { Cst.BracketIndex (Cst.EndSlice $3) }
  | LBRACKET NUMBER_CONSTANT COLON NUMBER_CONSTANT RBRACKET { Cst.BracketIndex (Cst.StartEndSlice ($2, $4)) }
  ;
