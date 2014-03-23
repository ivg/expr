open Printf
open Ast

type expr_type =  
    Number | Bool | Symbol | NumArray | SymArray | Poly of int 

exception Error of Ast.t * expr_type * expr_type

let bprint_type buf ty = bprintf buf "%s" begin match ty with
  | Number    -> "Number"
  | Symbol    -> "Symbol"
  | NumArray  -> "Number[]"
  | SymArray  -> "Symbol[]"
  | Poly n    -> "T"^(string_of_int n)
  | Bool      -> "Bool"
end

module Print = Printer.Make
  (struct type t = expr_type let bprint = bprint_type end)


let error context expected provided =
  raise (Error (context,expected,provided))

module Args : sig
  type t 
  val empty: t
  val arity: t -> int
  val nth: int -> t -> expr_type
  val set: int -> expr_type -> t -> t
  val get: int -> t -> expr_type
  val bprint: Buffer.t -> t -> unit
end = struct 
  module Map =
    Map.Make(struct type t = int let compare = compare end)

  type t = expr_type Map.t 
  let empty = Map.empty

  let get idx db =
    if not (Map.mem idx db) then Poly idx else
      match Map.find idx db with
        | Poly idx' when idx' <> idx && Map.mem idx' db ->
            Map.find idx' db
        | ty -> ty 

  let arity db = Map.fold (fun n _ n' -> max n n') db 0
  let nth = get

  let set idx ty db =
    let idx = match get idx db with
      | Poly idx -> idx
      | _ -> idx in
    let db = match ty, get idx db with
      | Poly n, Poly idx' when n <> idx  -> if idx = idx' 
        then Map.add (-idx) (Poly (-n)) (Map.add idx (get n db) db)
        else  error (Ref idx) ty (Poly idx')
      | ty, Poly idx | Poly idx, ty -> Map.add idx  ty db
      | ty,ty' when ty = ty' -> Map.add idx ty db
      | ty,ty'  -> error (Ref idx) ty ty' in 
    match ty, get (-idx) db with
      | NumArray, Poly idx -> Map.add idx Number   db
      | Number,   Poly idx -> Map.add idx NumArray db
      | SymArray, Poly idx -> Map.add idx Symbol   db
      | Symbol,   Poly idx -> Map.add idx SymArray db
      | NumArray, Number | SymArray, Symbol
      | Number, NumArray | Symbol, SymArray
      | _, Poly _  | Poly _,_   -> db
      | ty,ty'  -> error (Ref idx) ty ty'

  let bprint buf db =
    let print = function
      | Poly n -> bprintf buf "%a -> " bprint_type (get n db)
      | ty     -> bprintf buf "%a -> " bprint_type ty in
    let rec iter n =
      if n <= arity db then begin print (get n db); iter (n+1) end in
    iter 1
end


module StringableType = struct
  type t = Args.t * expr_type
      
  let bprint buf (args,ty) =
    bprintf buf "%a%a" Args.bprint args bprint_type ty
end
      
module Stringable = Printer.Make(StringableType)
  
include Stringable

let return ty (args,ty') = args,ty
let drop (args,_) = args


let infer ast =
  let rec typeof args = function
    | Num _ -> args, Number
    | Sym _ -> args, Symbol
    | Self  -> args, Symbol
    | Idx   -> args, Number
    | Ref n -> begin match Args.get n args with
        | Poly n' -> if  n' < 0
          then error (Ref n) (Poly (~-n')) (Poly n')
          else Args.set n (Poly n') args, Poly n' 
        | ty -> args, ty end
    | UnaryArith (_,x) -> unify (args,Number) x
    | Arith (_,x,y) -> unify (unify (args,Number) x) y
    | ArrayOfNum exps ->
        return NumArray (List.fold_left unify (args,Number) exps)
    | ArrayOfSym exps -> 
        return SymArray (List.fold_left unify (args,Symbol) exps)
    | IfThenElse (cond,e1,e2) ->
        let args, ty = unify (unify (typeof args e1) e2) e2 in
        return ty (unify_bool args cond)
    | ArraySize a -> let args,_ = unify_elem args a in args,Number
    | ArrayElementIdx a | ArrayElement (a,_) -> unify_elem args a
  and unify (args,ty) exp =
    let args, ty' = typeof args exp in match ty,ty' with
    | Poly n, ty | ty, Poly n -> begin match Args.get n args, ty with
        | Poly n, Poly n' -> Args.set n' (Poly n) args, Poly n
        | Poly n, ty | ty, Poly n -> Args.set n ty args, ty
        | ty,ty' when ty = ty' -> args, ty
        | ty,ty' -> error exp ty ty' end
    | ty, ty' when ty = ty' -> args,ty
    | ty, ty' -> error exp ty ty'
  and unify_elem args arr =
    let args, ty = typeof args arr in match ty with
    | NumArray -> args, Number
    | SymArray -> args, Symbol
    | Poly n -> begin match Args.get n args, Args.get (-n) args with
        | NumArray,Number -> args, Number
        | SymArray,Symbol -> args, Symbol
        | Poly n,  Number -> Args.set n NumArray args, Number
        | Poly n,  Symbol -> Args.set n SymArray args, Symbol
        | Poly n,  Poly n' when n <> n' && n' < 0 -> args, Poly n'
        | ty, ty' -> error arr ty ty' end
    | ty -> error arr NumArray ty
  and unify_bool args expr = match expr with
    | Relat (_,x,y)  -> begin match (unify (typeof args x) y) with
        | args, NumArray -> error ast Number NumArray
        | args, SymArray -> error ast Number SymArray
        | args,_ -> args,Bool end
    | Not x -> unify_bool args x
    | And (x,y) | Or (x,y) ->
        unify_bool (drop (unify_bool args x)) y in
  typeof Args.empty ast















