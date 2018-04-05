(* The intent of this task is to help you get acquainted with programming in 
Standard ML, and to gain practical experience working with abstract syntax trees
(ASTs) and symbol tables. Your task is to complete the implementation of an 
interpreter for a small language of integer and list expressions. This is done 
by completing the eval function in the handed-out file `listcomp.sml’. The 
interpreter follows the same structure as the one presented in the Interpretation
lecture notes. The semantics of the language you have to implement is as follows:

There are two kinds of values: integers (IntVal) and lists of values (ListVal).
    - Const v evaluates to v.
    - Var k evaluates to the value of the variable k in the symbol table.
    - List es first evaluates every expression in the list es, then constructs 
        a ListVal of the results.
    - Compr (e1, k, e2, NONE) is the simplest case of a list comprehension. It 
        first evaluates e2, which must produce a list l. Then it successively 
        binds k to each element of l, and evaluates e1 with the new binding 
        visible. The result is a list of the values produced by e1. This 
        corresponds to the Python list comprehension [e1 for k in e2].
    - Compr (e1, k, e2, SOME p) is like the previous case, but each element of 
        the list is first filtered by evaluating p with k bound to the element.
        Only if p evaluates to a non-zero integer is e1 invoked for that element.
        This corresponds to the Python list comprehension [e1 for k in e2 if p].
    - Range (e1, e2) evaluates e1 to an integer x, and e2 to an integer y. It 
        then produces the list of integers from x to y, both inclusive. The code 
        handout contains an SML function range that implements this for SML integers:
            - range 0 10 [];
            > val it = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10] : int list
    - The arithmetic operations have their usual semantics. Comparison operators 
        (Equal and Less) return 1 for true, and 0 for false. *)

