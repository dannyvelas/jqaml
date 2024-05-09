{
open Parser

exception SyntaxError of string
}

rule token = parse 
| "." { PERIOD }
