{
open Parser
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
  | id              { ID(Lexing.lexeme lexbuf) }
  | eof             { EOF }
  | "(*"            { comments 0 lexbuf }
and comments level = parse
  | "*)"            { if level = 0 then token lexbuf else comments (level-1) lexbuf }
  | _               { comments level lexbuf }
  | eof             { EOF }
