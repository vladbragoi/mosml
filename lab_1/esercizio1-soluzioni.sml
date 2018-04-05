(* alberi binari *)


datatype btree = Empty | leaf of int | node of int*btree*btree;

fun occ(Empty, n) = false |
    occ(leaf(x), n) = (x=n) |
    occ(node(x, t, u), n) = (x=n) 
                         orelse occ(t, n) orelse occ(u, n)


fun sm(Empty) = 0 |
    sm(leaf(n)) = n |
    sm(node(n,t,u)) = n+sm(t)+sm(u);


(* espressioni aritmetiche e valutazione *)

datatype aexp = c of int | s of aexp*aexp | p of aexp*aexp;
fun aeval (c(n)) = n |
    aeval (s(e1,e2)) = aeval(e1) + aeval(e2) |
    aeval (p(e1,e2)) = aeval(e1) * aeval(e2) ;

- aeval(s(c(3),p(c(2),s(c(1),c(2)))));


(* espressioni aritmetiche con operatori infissi e valutazione  *)

infixr 5 ++;
infixr 6 **;
datatype aexpi = c of int | ++ of aexpi*aexpi | ** of aexpi*aexpi;
fun aeval (c(n)) = n |
    aeval (e1 ++ e2) = aeval(e1) + aeval(e2) |
    aeval (e1 ** e2) = aeval(e1) * aeval(e2) ;

- aeval(c(2) ++ (c(3) ** c(4)));
- aeval(c(2) ++ c(3) ** c(4));
- aeval(c(3) ** c(4) ++ c(2));


