type id = Ts.Id.t
type v  = Ts.Value.t

exception Not_a_symbol of v
exception Not_a_number of v 
exception Not_a_scalar of v
exception Not_an_array of v
exception Uncomparable of v * v
exception Bad_argument of id
exception Bad_variable of int

val create: Ast.t -> int -> id -> id array -> (id -> v) -> v
(** [create ast idx self pars getenv] перобразует абстрактное
    синтаксическое дерево [ast] в окамловскую функцию, принимающую
    [idx] в качестве значения выражения [$#], [self] для выражения
    [self] и массив [pars]  аргументов выражения. Функция [getenv]
    служит для поиска значения по имени.
    
    
*)



















