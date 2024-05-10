{
open Parser

exception SyntaxError of string
}

rule token = parse 
| "." { PERIOD }
| "null" { NULL }
| "true" { TRUE }
| "false" { FALSE }
| ".." { RECURSE }
