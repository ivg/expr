module Env = Ts.Env
module Value = Ts.Value
open Value
open Printf

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
let env = Env.add !@"onoff" ty (Strings [| !@"enabled"; !@"disabled"|]) env
let bl  = [|
  !@"work_freq";
  !@"angle";
  !@"sampling_rate";
  !@"period";
  !@"delay";
  !@"sector";
  !@"antenna";
  !@"onoff"
|]

let expr1_s = "if $3 = 1e3 then (2-3) else (2+3)"
let expr1_s = "3e8 * pi / ($1**0.5 * 1e4)"
let expr1_s = "if $1 = $3 then $2 else $3"
let expr1_s = "3e8 * pi / ($1**0.5 * 1e4)"
let expr1_s = "$1[0]"
let expr1_s = "if $3 < $1 and not $3 = antenna then 2 else 3"
let expr1_s = "3e8 / if $2 in angle.elevation then ($1**0.5 * 1e4) / pi else $1 / 0.5**0.5"
let expr1_s = "if $6[1] + 1 < 0 then $6 else {2,3}"
let expr1_s = "if $1[0] < $3 then $1 else $2"
let expr1_s = "3e8 / if $2 in antenna then ($1**0.5 * 1e4) / pi else $1 / 0.5**0.5"
let expr1_s = "if $5 in antenna.circular then 2 else (12*2)/4"
let expr1_s = "if {: a,b,c :}[0] in $2 then {1,2} else {1, 2+3}"
let expr1_s = "2 * 2 + (2 * 3 - $1[0])"
let expr1_s = "if $6[1] + 1 < 0 then $6 else {2,3}"
let expr1_s = "if $1 < $2 and $2 > 0 then $3 else $4+1"

let ast = Parser.expr Lexer.tokens (Lexing.from_string expr1_s)

let ety = Type.infer ast 

let () = Type.print ety
let () = print_newline ()

let expr1 = Ocaml.create ast

let env = Env.add !@"expr" ty (Closure (expr1 0 !@"x" bl)) env

let result = Env.get_value !@"expr" env

                   
let _ = print_string (Value.to_string result)
(* let _ = print_string (Value.to_string (expr1 bl Env.get_value)) *)



















