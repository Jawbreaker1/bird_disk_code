(* BirdDisk v0.1 EBNF *)
(* Runtime errors (e.g., division by zero) are specified in SPEC.md. *)

program       = { function } ;

function      = "rule" ident "(" [ params ] ")" "->" type ":" { stmt } "end" ;

params        = param { "," param } ;
param         = ident ":" type ;

type          = base_type { "[]" } ;
base_type     = "i64" | "bool" ;

stmt          = set_stmt
              | put_stmt
              | yield_stmt
              | when_stmt
              | repeat_stmt
              ;

set_stmt      = "set" ident [ ":" type ] "=" expr "." ;
put_stmt      = "put" ident [ "[" expr "]" ] "=" expr "." ;
yield_stmt    = "yield" expr "." ;

when_stmt     = "when" expr ":" { stmt } "otherwise" ":" { stmt } "end" ;

repeat_stmt   = "repeat" "while" expr ":" { stmt } "end" ;

expr          = logic_or ;

logic_or      = logic_and { "||" logic_and } ;
logic_and     = equality { "&&" equality } ;
equality      = compare { ( "==" | "!=" ) compare } ;
compare       = add { ( "<" | "<=" | ">" | ">=" ) add } ;
add           = mul { ( "+" | "-" ) mul } ;
mul           = unary { ( "*" | "/" | "%" ) unary } ;
unary         = ( "!" | "-" ) unary | postfix ;

postfix       = primary { "[" expr "]" } ;

primary       = int_lit
              | bool_lit
              | array_lit
              | array_new
              | call_or_ident
              | "(" expr ")"
              ;

call_or_ident = ident [ "(" [ args ] ")" ] ;
args          = expr { "," expr } ;

array_lit     = "[" [ args ] "]" ;
array_new     = "array" "(" expr ")" ;

bool_lit      = "true" | "false" ;
int_lit       = digit { digit } ;

ident         = letter { letter | digit | "_" } ;

letter        = "A"…"Z" | "a"…"z" ;
digit         = "0"…"9" ;
