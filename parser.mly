%{
  open Ast
  open Ts
  open Value

%}

%token EOF MINUS PLUS DIV MUL POW EQ LESS LESSEQ GREAT GREATEQ SELF
%token REF AND OR LBR RBR IF THEN ELSE ABS SEC ELSEP EOF DOT IDX SIZE
%token LFLOOR RFLOOR LCEIL RCEIL LPAR RPAR LCURC RCURC LCUR RCUR NOT
%token LOG 
%token <float> PI E FLOAT
%token <int>   INT 
%token <string> CAT TXT

%nonassoc IF THEN ELSE
%left OR
%left AND
%left EQ LESS LESSEQ GREAT GREATEQ
%nonassoc SEC
%left PLUS MINUS
%left MUL DIV 
%right POW LOG
%nonassoc UMINUS
%nonassoc USIZE
%nonassoc NOT
%nonassoc LPAR RPAR LBR RBR FLOAT  INT LCEIL RCEIL CAT

%start expr
%type <Ast.t> expr

%%

expr:
  | LPAR expr RPAR {$2}
  | REF INT {Ref $2}
  | MINUS expr %prec UMINUS {UnaryArith (Neg,$2)}
  | SIZE  expr %prec USIZE  {ArraySize $2}
  | FLOAT {Num (Float $1)}
  | INT   {Num (Float (float_of_int $1))}
  | CAT   {Sym (Id (Id.of_string $1))}
  | TXT   {Sym (Text $1)}
  | IDX   {Idx}
  | SELF  {Self}
  | ABS expr ABS       {UnaryArith (Abs,$2) }
  | LFLOOR expr RFLOOR {UnaryArith (Floor, $2) }
  | LCEIL  expr RCEIL  {UnaryArith (Ceil,$2) }
  | expr PLUS  expr    {Arith (Plus, $1,$3)}
  | expr MINUS expr    {Arith (Minus, $1,$3)}
  | expr MUL expr      {Arith (Mul, $1,$3)}
  | expr DIV expr      {Arith (Div, $1,$3)}
  | expr POW expr      {Arith (Pow, $1,$3)}
  | expr LOG expr      {Arith (Log, $1,$3)}
  | expr SEC expr      {Arith (Sec, $1,$3)}
  | IF bool_expr THEN expr ELSE expr {IfThenElse ($2, $4, $6)}
  | expr LBR INT RBR { ArrayElement ($1, $3)}
  | expr LBR IDX RBR { ArrayElementIdx $1}
  | LCUR  array RCUR { ArrayOfNum  $2}
  | LCURC categories RCURC {ArrayOfSym $2}
;

bool_expr: 
  | LPAR bool_expr RPAR {$2}
  | NOT bool_expr { Not $2 }
  | bool_expr AND bool_expr {And ($1,$3)}
  | bool_expr OR  bool_expr {Or ($1,$3)}
  | expr EQ      expr {Relat (Eq, $1, $3)}      
  | expr LESS    expr {Relat (Less, $1, $3)}    
  | expr GREAT   expr {Relat (Great, $1, $3)}   
  | expr GREATEQ expr {Relat (GreatEq, $1, $3)} 
  | expr LESSEQ  expr {Relat (LessEq, $1, $3)} 
;

array:
  | expr {[$1]}
  | expr ELSEP array {$1 :: $3}
 ;

categories:
  | expr {[$1]}
  | expr ELSEP categories {$1 :: $3}
;
