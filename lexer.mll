{
open Parser

let get = Lexing.lexeme
}
let alpha = [ 'A'-'Z' 'a'-'z' ]
let other = [ '0'-'9' '_']
let id = alpha(alpha|other)*

rule token = parse
  | [' ' '\t']      { token lexbuf }
  | '\n'            { NEWLINE }
  | '='             { EQUAL }
  | '('             { LPAR }
  | ')'             { RPAR }
  | '['             { LBRAC }
  | ']'             { RBRAC }
  | ','             { COMMA }
  | id              { ID(get lexbuf) }
  | eof             { EOF }

