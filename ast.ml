type unaryop = Abs  | Ceil  | Floor | Neg 
type arithop = Plus | Minus | Div   | Mul    | Sec    | Pow | Log
type relatop = Eq   | Less  | Great | LessEq | GreatEq 

type v = Ts.Value.t

type t =
  | Idx | Self
  | Num   of v
  | Sym   of v
  | Ref   of int
  | Arith of arithop * t * t
  | UnaryArith of unaryop * t
  | ArrayElement of t * int
  | ArraySize    of t
  | ArrayElementIdx of t
  | ArrayOfNum of t list
  | ArrayOfSym of t list
  | IfThenElse of bool_expr * t * t
and bool_expr =
  | Not of bool_expr
  | Relat of relatop * t * t
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr


let rec to_string ast =
  let to_string = function
    | IfThenElse _ | Arith _ as exp -> "(" ^ to_string exp ^ ")"
    | exp -> to_string exp in
  let string_of_arithop = function
    | Plus  -> " + "
    | Minus -> " - "
    | Div   -> " / "
    | Mul   -> " * "
    | Sec   -> " ^ "
    | Pow   -> "**"
    | Log   -> "//" in
  let string_of_unaryop ast = function
    | Abs  -> "|" ^ to_string ast ^ "|"
    | Ceil -> "|^ " ^ to_string ast ^ " ^|"
    | Floor -> "|_ " ^ to_string ast ^ " _|" 
    | Neg  -> "-(" ^ to_string ast ^ ")" in
  let string_of_elements lst =
    let b = Buffer.create 100 in
    let _ = List.fold_left (fun sep a ->
      Printf.bprintf b "%s%s" sep (to_string a); ", ") "" lst in
    Buffer.contents b in
  let string_of_relat = function
    | Eq     -> " = " 
    | Less   -> " < " 
    | Great  -> " > " 
    | LessEq -> " <= "
    | GreatEq -> " >= " in
  let rec string_of_bexp = function
    | Not exp -> "not ("^ string_of_bexp exp ^")"
    | Relat (op,a,a')->
        to_string a ^ string_of_relat op ^ to_string a'
    | And (e,e') ->
        "("^ string_of_bexp e ^") and (" ^ string_of_bexp e' ^")" 
    | Or (e,e') ->
        "("^ string_of_bexp e ^") or (" ^ string_of_bexp e' ^")" in
  let string_of_elem = function
    | Idx -> "$$"
    | Self -> "$?"
    | Sym v | Num v -> Ts.Value.to_string v
    | Ref n -> "$" ^ string_of_int n
    | Arith (op,a,a') ->
        to_string a ^ string_of_arithop op ^ to_string a'
    | UnaryArith (op,a) -> string_of_unaryop a op
    | ArrayElement (a,n) -> to_string a ^ "[" ^ string_of_int n ^ "]"
    | ArraySize a -> "#" ^ to_string a
    | ArrayElementIdx a -> to_string a ^"[$$]"
    | ArrayOfNum lst -> "{"  ^ string_of_elements lst ^ "}"
    | ArrayOfSym lst -> "{:" ^ string_of_elements lst ^ ":}"
    | IfThenElse (bexp, a, a') ->
        "if " ^ string_of_bexp bexp ^ " then " ^ to_string a ^
          " else " ^ to_string  a' in
  string_of_elem ast
  























