(******************************************)
(*     ZADANIE SORTOWANIE TOPOLOGICZNE    *)
(*        ROZWIĄZANIE: MARCIN ŻOŁEK       *)
(*          RIWJU: PAWEŁ PILARSKI         *)
(******************************************)

exception Cykliczne

let topol lista = 
    (** utworzenie grafu w postaci mapy list *)
    let tworz_graf lista = 
        let pom a h =
            (** mapa po przetworzeniu sąsiadów wierzchołka *)
            let b = List.fold_left (fun a h -> if PMap.mem h a then a else PMap.add h [] a) a (snd h) in 
            (** dołączenie sąsiadów wierzchołka do mapy, w której już jest wierzchołek *) 
            if PMap.mem (fst h) b then PMap.add (fst h) ((snd h) @ (PMap.find (fst h) b)) b  
            (** dołączenie sąsiadów wierzchołka do mapy, w której jeszcze nie ma wierzchołka *) 
            else PMap.add (fst h) (snd h) b  
        in
        List.fold_left pom PMap.empty lista
    in
    let graf = tworz_graf lista in 
    (** utworzenie mapy liczb, która reprezentuje stany wierzchołków:
    0 <=> nieprzetworzony; 1 <=> w trakcie przetwarzania; 2 <=> przetworzony *)
    let tworz_stan lista = 
        let pom a h =
            (** mapa po przetworzeniu sąsiadów wierzchołka *)
            let b = List.fold_left (fun a h -> PMap.add h 0 a) a (snd h) in  
            (** dołączenie wierzchołka do mapy *)
            PMap.add (fst h) 0 b 
        in
        List.fold_left pom PMap.empty lista
    in
    (** przeszukiwanie grafu w głąb *)
    let rec dfs (stan, porzadek) wierzcholek = 
        (** lista wierzchołków, do których jest krawędź z wierzchołka *)
        let sasiedzi = PMap.find wierzcholek graf in 
        (** stan wierzchołka *)
        let kolor = PMap.find wierzcholek stan in 
        match kolor with
        | 0 -> 
            let (stan, porzadek) = List.fold_left (fun a h -> dfs a h) (PMap.add wierzcholek 1 stan, porzadek) sasiedzi in
            (PMap.add wierzcholek 2 stan, wierzcholek :: porzadek) 
        | 1 -> raise Cykliczne 
        | _ -> (stan, porzadek)
    in
    let stan = tworz_stan lista in
    (* przejście po wszystkich wierzchołkach początkowych z listy *)
    let (_, porzadek) = List.fold_left (fun a (h, _) -> if PMap.find h stan = 0 then dfs a h else a) (stan, []) lista in
    porzadek 
