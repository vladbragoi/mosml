(* Define a datatype `btree’ representing binary trees.
Then write the following functions:
occ: btree * int → bool
This function decides whether an integer occurs in a tree or not.
sm: btree → int
This function returns the sum of all the labels of a binary tree of integers. *)

datatype btree = leaf of int 
            | node of int * btree * btree;

val B = 
    node(4, 
        node(3, leaf(1), leaf(2)),
        node(5, leaf(6), leaf(7))
);

fun occ(leaf(n), x) = (n = x)
    | occ(node(n, t1, t2), x) = (x = n) orelse occ(t1, x) orelse occ(t2, x);
    
fun sum(leaf(n)) = n
    | sum(node(n, t1, t2)) = n + sum(t1) + sum(t2);