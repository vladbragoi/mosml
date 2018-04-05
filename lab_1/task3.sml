(* Define a datatype representing arithmetic expressions with constants, variables, 
sum and product. Define an evaluator for expressions of this type. *)

datatype Exp = var of int
            | sum of Exp * Exp
            | prod of Exp * Exp;

fun eval(var(n)) = n
    | eval(sum(e1, e2)) = eval(e1) + eval(e2)
    | eval(prod(e1, e2)) = eval(e1) * eval(e2);