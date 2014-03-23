module Env = Ts.Env
module Value = Ts.Value
open Value
open Printf
open OUnit

let _ = Parsing.set_trace false

let (!@) = Ts.Id.of_string
let ty  = Ts.Type.make_undefined !@"type"
let env = Env.empty
let env = Env.add !@"work_freq" ty (Float 15e6) env
let env = Env.add !@"angle"  ty  (Id !@"angle.elevation") env
let env = Env.add !@"sampling_rate" ty (Float 100e3) env
let env = Env.add !@"period" ty (Float 20e-3) env
let env = Env.add !@"delay" ty (Float 5e-3) env
let env = Env.add !@"sector" ty (Array [|0.; 3.14|]) env
let env = Env.add !@"antenna" ty (Id !@"antenna.circular.center") env
let args  = [|
  !@"work_freq";                        (* 1 *)
  !@"angle";                            (* 2 *)
  !@"sampling_rate";                    (* 3 *)
  !@"period";                           (* 4 *)
  !@"delay";                            (* 5 *)
  !@"sector";                           (* 6 *)
  !@"antenna";                          (* 7 *)
|]


let exec str expect ()=
  let ast = Parser.expr Lexer.tokens (Lexing.from_string str) in
  let ity = Type.infer ast in
  let exp = Ocaml.create ast in
  let env = Env.add !@"expr" ty
    (Closure (exp 0 !@"ctx" args)) env in
  let res = Env.get_value !@"expr" env in
  let msg = sprintf "%s: %s ≡ %s ≠ %s"
    str (Type.to_string ity)
    (Value.to_string expect) (Value.to_string res) in
  assert_equal ~msg res expect

let exec_rev str expect () =
  let ast = Parser.expr Lexer.tokens (Lexing.from_string str) in
  let str' = Ast.to_string ast in
  exec str' expect ()

exception Bad
let bad str () =
  let ast = Parser.expr Lexer.tokens (Lexing.from_string str) in
  let msg = sprintf "expr '%s' shouldn't be well typed"  str in
  let infer () =
    try Type.infer ast with Type.Error _ -> raise Bad in
  assert_raises ~msg Bad infer

let check_type str n ty () =
  let ast = Parser.expr Lexer.tokens (Lexing.from_string str) in
  let args,_  = Type.infer ast in
  let ty' = Type.Args.nth n args in
  let msg = sprintf "parameter $%d must have type %s. Infered %s"
    n (Type.Print.to_string ty)  (Type.Print.to_string ty') in
  assert_equal ~msg ty ty'
  
let e1 = "if $1 < $2 and $2 > 0 then $3 else $4+1"
let e2 = "if $1[0] < $2 then $3 else $1[1]+3"
let e3 = "if $? in $1 and $2 = $$ then $3 else $2"
let e4 = "if #$1 = $2 then $1[0] else $1[1]"
let suite =
  "Expr" >::: [
    "0" >:: exec "100//10" (Float 2.);
    "1" >:: exec "2 * 2 + (2 * 3 - 5)" (Float 5.);
    "2" >::
      exec "{:hello, world:}" (Ids [|!@"hello"; !@"world"|]);
    "3" >::
      exec "3e8 / if $2 in angle.elevation then 1e8 else 1e-8"
      (Float 3.);
    "4" >:: exec "if pi / 2 > pi / 4 then pi else pi / 0"
      (Float (4. *. atan 1.));
    "5" >:: exec "2**2-2 " (Float 2.);
    "6" >:: exec "2**(2-2)" (Float 1.);
    "7" >:: exec "|_ pi _|" (Float 3.);
    "8" >:: exec "|^ pi ^|" (Float 4.);
    "9" >:: exec "|{2+2, 3-3,4-5}[2]|" (Float 1.);
    "a" >:: exec "if 2 < 2 or 2 > 2 then 0/0 else 1" (Float 1.);
    "b" >:: exec "if 2*2 = 4 or not 2*2 = 4 then 1 else 0/0" (Float 1.);
    "c" >:: exec "if antenna.circular in antenna then 8 else 7" (Float 8.);
    "d" >:: exec "$4 * $3 * 2" (Float 4e3);
    "d1">:: exec "#{1,2,3}" (Float 3.);
    "d2">:: exec "#{:a,b,c:}" (Float 3.);
    "d3">:: exec "-#{:a,b,c:}" (Float ~-.3.);
    "d4">:: exec "#$6" (Float 2.);
    "d5">:: exec "#$6 + #{1,2} - 2 * 2" (Float 0.);
    "e" >:: bad "if 2 < 2 then symbol else 2";
    "f" >:: bad "if $1 < $2 then $1[0] else $2";
    "g" >:: bad "if $1 > 0 then symbol else $1";
    "h" >:: bad "if $1 in symbol then $1 else 2";
    "i" >:: bad "3e8 / if $1 in symbol then $1 else $2";
    "k" >:: bad "{: if $1 < $2 and $2 > 0 then $1 else $3 :}";
    "l" >:: bad "if $1 < $2 and $2 > 0 then $1 else ok";
    "l" >:: bad "if #$6 in something then nothing else everything";
    "m" >:: check_type e1 1 Type.Number;
    "n" >:: check_type e1 2 Type.Number;
    "o" >:: check_type e1 3 Type.Number;
    "p" >:: check_type e1 4 Type.Number;
    "q" >:: check_type e2 1 Type.NumArray; 
    "r" >:: check_type e2 2 Type.Number; 
    "s" >:: check_type e2 3 Type.Number; 
    "s1">:: check_type e4 2 Type.Number;
    "t" >:: exec "if $? in generator then 0/0 else $6[$$]"
      (Float 0.);
    "u" >:: check_type e3 1 Type.Symbol;
    "v" >:: check_type e3 2 Type.Number;
    "w" >:: check_type e3 3 Type.Number;
    "a1" >:: exec_rev "2 * 2 + (2 * 3 - 5)" (Float 5.);
    "a2" >::
      exec_rev "{:hello, world:}" (Ids [|!@"hello"; !@"world"|]);
    "a3" >::
      exec_rev "3e8 / if $2 in angle.elevation then 1e8 else 1e-8"
      (Float 3.);
    "a5" >:: exec_rev "2**2-2 " (Float 2.);
    "a6" >:: exec_rev "2**(2-2)" (Float 1.);
    "a7" >:: exec_rev "|_ pi _|" (Float 3.);
    "a8" >:: exec_rev "|^ pi ^|" (Float 4.);
    "a9" >:: exec_rev "|{2+2, 3-3,4-5}[2]|" (Float 1.);
    "aa" >:: exec_rev "if 2 < 2 or 2 > 2 then 0/0 else 1" (Float 1.);
    "ab" >:: exec_rev "if 2*2 = 4 or not 2*2 = 4 then 1 else 0/0" (Float 1.);
    "ac" >:: exec_rev "if antenna.circular in antenna then 8 else 7"
      (Float 8.);
    "ad" >:: exec_rev "$4 * $3 * 2" (Float 4e3);
  ]
    



















