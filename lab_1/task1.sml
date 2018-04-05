(* OPERAZIONI DI BASE IN MOSML *)

load "Lib";         (* Importare libreria aggiuntiva *)
val x  = 8;         (* Dichiarazione di variabile e assegnamento *)
hd[5,2,1,7,8];      (* Head di una lista. E' possibile passare i parametri anche senza parentesi tonde *)
hd([5,2,1,7,8]);
tl[5,2];            (* Tail di una lista. *)
tl([5,2]);
1::[2,3];           (* Aggiunta di un elemento ad una lista *)
[1,5]@[2,3];        (* Concatenazione di liste *)
[1,2] = [1,2,3];    (* Confronto tra liste (vale l'ordine e il tipo di dato) *)
length [2,3,4];     (* Lunghezza di una lista *)
rev [4,3,2];         (* Reverse di una lista *)
val addOne = fn x => x + 1;     (* Dichiarazione e definizione di funzione *)
fun AddOne x = x + 1;           (* Forma contratta di dichiarazione e definizione di funzione *)
val anotherAddOne = addOne;     (* Alias di una funzione *)
anotherAddOne(5);               (* Chiamata di funzione. *)
anotherAddOne 5;                (* Come sopra *)
let val x = 10 in x * x end;    (* Dichiarazione privata *)

let
    val x = 10
in 
    x * x
end;                            (* Come sopra *)

fun mystery(L) =
    if L = nil then
        L
    else
        mystery(tl(L)) @ [hd(L)];       (* Funzione ricorsiva: reverse di una lista *)

fun keepFirstN(x, L) =
    if x <= 0 orelse L = nil then
        nil
    else
        [hd(L)] @ keepFirstN(x-1, tl(L));   (* Funzione che seleziona i primi N elementi di una lista *)
keepFirstN(3,["a","b","c","d","e","f","g","h","i"]);

fun keepFirstN(0, _) = nil              
    | keepFirstN(x, L) = [hd(L)] @ keepFirstN(x-1, tl(L));  (* Come sopra, con mattern matching *)

fun mystery([]) = []
    | mystery(start::rest) =
        mystery(rest) @ [start:int];        (* List reverse con pattern matching *)

fun fact(1) = 1
    | fact(n) = n*fact(n-1);                (* Fattoriale con pattern matching *)

load "Int";                                 (* Carica il modulo Int (per poter usare il metodo toString *)
let fun loop(count) = (
    print("Count = ");
    print(Int.toString(count));
    print("\n");
    if count < 10 then
        loop(count+1)
    else
        count)
    in
        loop(0)
end;                                        (* Iterazione ricorsiva *)