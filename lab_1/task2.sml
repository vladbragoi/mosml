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