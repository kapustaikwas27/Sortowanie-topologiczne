(** Sortowanie topologiczne *)

exception Cykliczne
(** wyjatek rzucany przez [topol] gdy zaleznosci sa cykliczne *)

val topol : ('a * 'a list) list -> 'a list 
(** Dla danej listy [(a_1,[a_11;...;a_1n]); ...; (a_m,[a_m1;...;a_mk])] 
    zwraca liste, na ktorej kazdy z elementow a_i oraz a_ij wystepuje
    dokladnie raz i ktora jest uporzadkowana w taki sposob, ze kazdy
    element a_i jest przed kazdym z elementow a_i1 ... a_il *)

(* Do wyświetlania grafu *)
(* val topol : (int * int list) list -> int list *) 
(* Po 26 linijce dodać *)
(* PMap.iter (fun w s -> begin print_int w; print_char ':'; List.iter (fun h -> print_int h; print_char ' ') s; print_newline (); end) graf; *)
