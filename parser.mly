%{
open Tools
%}

%token NEWLINE EQUAL LPAR RPAR LBRAC RBRAC COMMA EOF
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
  | ID parameters EQUAL term EOF        { ($1, $2, $4) }

parameters:
  |                         { [] }
  | parameter parameters    { $1::$2 }

atomic_parameter:
  | ID                          { Var($1) }
  | ID LBRAC RBRAC              { Constr($1,Tuple([]))}
  | ID LBRAC parameter RBRAC    { Constr($1,$3)}
  | LPAR RPAR                   { Tuple([]) }
  | LPAR parameter RPAR         { $2 }

parameter:
  | atomic_parameter                    { $1 }
  | atomic_parameter parameter_tuple    { Tuple($1::$2) }

parameter_tuple:
  | COMMA atomic_parameter                  { [$2] }
  | COMMA atomic_parameter parameter_tuple  { $2::$3 }

atomic_terms:
                                { [] }
  | atomic_term atomic_terms    { $1::$2 }

atomic_term:
  | ID                      { Var($1) }
  | ID LBRAC RBRAC          { Constr($1,Tuple([])) }
  | ID LBRAC term RBRAC     { Constr($1,$3) }
  | LPAR RPAR               { Tuple([]) }
  | LPAR term RPAR          { $2 }

app:
  | atomic_term             { $1 }
  | app atomic_term        { App($1,$2) }

term:
  | app             { $1 }
  | app term_tuple  { Tuple($1::$2) }

term_tuple:
    COMMA app               { [$2] }
  | COMMA app term_tuple    { $2::$3 }
