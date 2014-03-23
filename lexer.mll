{
  open Lexing
  open Parser
}

let char  = ['a'-'z']
let digit = ['0'-'9']
let float =  ['0'-'9'] ['0'-'9' '_']*
  ('.' ['0'-'9' '_']* )?
  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?

let integer  = digit+
let position = digit+
let paref = ['1'-'9'] digit*
let category =  char (char | "_" | ".")*

rule tokens = parse
  | eof {EOF}
  | integer as lxm {INT (int_of_string lxm)}
  | float as lxm {FLOAT (float_of_string lxm)}
  | "pi" {FLOAT (4. *. atan 1.)}
  | "e"  {FLOAT (sinh 1. +. cosh 1.)}
  | "-"  {MINUS}
  | "+"  {PLUS}
  | "/"  {DIV}
  | "//" {LOG}
  | "*"  {MUL}
  | "**" {POW}
  | "="  {EQ}
  | "<"  {LESS}
  | "<="| "in" {LESSEQ}
  | ">"  {GREAT}
  | ">=" {GREATEQ}
  | "$$" {IDX}
  | "$?" {SELF}
  | "$"  {REF}
  | "and" {AND}
  | "or" {OR}
  | "("  {LPAR}
  | ")"  {RPAR}
  | "["  {LBR}
  | "]"  {RBR}
  | "|_" {LFLOOR}
  | "_|" {RFLOOR}
  | "|^" {LCEIL}
  | "^|" {RCEIL}
  | "|"  {ABS}
  | "{:" {LCURC}
  | ":}" {RCURC}
  | "^"  {SEC}
  | "{"  {LCUR}
  | "}"  {RCUR}
  | "#"  {SIZE}
  | "not" {NOT}
  | "if" {IF}
  | "then" {THEN}
  | "else" {ELSE}
  | ","    {ELSEP}
  | category as lxm {CAT lxm }
  | '"' ((_ # '"')* as txt) '"' {TXT txt}
  | '\n'   {new_line lexbuf; tokens lexbuf}
  | [' ' '\t'] {tokens lexbuf}
{}

















