/* file: haskell.mly */

%{
open Tools
open Printf

%}

%token NEWLINE EQUAL LPAR RPAR LBRAC RBRAC EOF COMMA
%token <string> ID

%start defs
%type <Tools.clause list> defs
%type <Tools.clause> def

%%

defs:
  | NEWLINE defs    { $2 }
  | def defs        { $1::$2 }
  | EOF             { [] }

def:
  | ID parameters EQUAL term NEWLINE    { ($1, $2, $4) }
  | ID parameters EQUAL term EOF        { failwith "the file must end with a newline!" }

parameters:
  |                                         { [] }
  | parameter parameters                    { $1::$2 }

parameter:
  | ID                          { Var($1) }
  | ID LBRAC RBRAC              { Constr($1,Tuple([]))}
  | ID LBRAC parameter RBRAC    { Constr($1,$3)}
  | LPAR parameter RPAR         { $2 }
  | parameter parameter_tuple   { Tuple($1::$2) }

parameter_tuple:
  | COMMA parameter                 { [$2] }
  | COMMA parameter parameter_tuple { $2::$3 }


atomic_terms:
                                { [] }
  | atomic_term atomic_terms    { $1::$2 }

atomic_term:
    ID                      { Var($1) }
  | ID LBRAC RBRAC          { Constr($1,Tuple([])) }
  | ID LBRAC term RBRAC     { Constr($1,$3) }
  | LPAR term RPAR          { $2 }

term:
    atomic_term         { $1 }
  | term atomic_term    { App($1,$2) }
  | term term_tuple     { Tuple($1::$2) }

term_tuple:
    COMMA term              { [$2] }
  | COMMA term term_tuple   { $2::$3 }

