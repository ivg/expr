open Ts
open Batteries
open Ast
open Value

type v = Value.t
type id = Id.t

exception Not_a_symbol of v
exception Not_a_number of v 
exception Not_a_scalar of v
exception Not_an_array of v
exception Uncomparable of v * v
exception Bad_argument of id
exception Bad_variable of int

let arc_length x y =
  let x,y = Pair.map Angle.of_radian (x,y) in
  Sector.radian_of_length (Sector.horizontal x y)

let unary op x =
  let x = match x with
    | Float x -> x
    | v -> raise (Not_a_number v) in
  match op,x with
    | Abs, x  -> abs_float x
    | Ceil,x  -> ceil x
    | Floor,x -> floor x
    | Neg, x  -> ~-. x

let arith op x y = 
  let make_op = function
    | Plus  -> ( +. )
    | Minus -> ( -. )
    | Div   -> ( /. )
    | Mul   -> ( *. )
    | Sec   -> arc_length
    | Pow   -> ( ** )
    | Log   -> (fun x b -> log x  /. log b) in
  match (x,y) with
    | Float x, Float y -> (make_op op) x y
    | Float _, v | v, Float _ | v,_ -> raise (Not_a_number v)

let getelem i = function
  | Array a   -> Float a.(i)
  | Ids a     -> Id a.(i)
  | v -> raise (Not_an_array v)

let getsize a =
  let size = match a with
    | Array a -> Array.length a
    | Ids   a -> Array.length a
    | v       -> raise (Not_an_array v) in
  Float (float_of_int size)

let relat op x y =
  let cmp_float = function
    | Eq -> (=)
    | Less -> (<)
    | Great -> (>)
    | LessEq -> (<=)
    | GreatEq -> (>=) in
  let cmp_id = function
    | Eq -> Id.equal
    | Less -> Id.base_of
    | Great -> Id.derived_from
    | LessEq -> flip Id.is_of_type 
    | GreatEq -> Id.is_of_type in
  match (x,y) with
    | Float x, Float y -> (cmp_float op) x y
    | Id x, Id y       -> (cmp_id op)    x y
    | v1,v2 -> raise (Uncomparable (v1,v2))

        
let make_array lst bl env =
  let float_of_expr (_,expr) = match expr bl env with
    | Float v -> v
    | v -> raise (Not_a_number v) in
  Array (Array.of_list (List.map float_of_expr lst))

let make_strings lst bl env =
  let id_of_expr (_,expr) = match expr bl env with
    | Id id -> id
    | id -> raise (Not_a_symbol id) in
  Ids (Array.of_list (List.map id_of_expr lst))

let float_of_expr = function
  | Float v -> v
  | v  -> raise (Not_a_number v)

let id_of_expr = function
  | Id id -> id
  | v -> raise (Not_a_number v)

let make_array f get lst =
  Array.of_list (List.map (fun e -> f (get e)) lst)

let safe_get getenv par n = 
  try getenv par.(n-1) with
    | Invalid_argument _ -> raise (Bad_variable n)
    | Not_found -> raise (Bad_argument par.(n-1))

let create ast idx self par getenv =
  let rec v = function
    | Num x | Sym x -> x
    | Idx -> (Float (float_of_int idx))
    | Self -> Id self
    | Ref n -> safe_get getenv par n 
    | Arith (op,x,y)     -> Float (arith op (v x) (v y))
    | UnaryArith (op,x)  -> Float (unary op (v x))
    | ArrayElement (a,i) -> getelem i (v a)
    | ArrayElementIdx a  -> getelem idx (v a)
    | ArraySize  a       -> getsize (v a)
    | ArrayOfNum exprs   -> Array (make_array float_of_expr v exprs)
    | ArrayOfSym exprs   -> Ids   (make_array  id_of_expr v exprs)
    | IfThenElse (cond,e1,e2) -> if b cond then v e1 else v e2
  and b = function
    | Not e -> not (b e)
    | Relat (op,x,y) -> relat op (v x) (v y)
    | And   (x,y)    -> (b x) && (b y)
    | Or    (x,y)    -> (b x) || (b y) in
  v ast




















