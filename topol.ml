(******************************************)
(*     ZADANIE SORTOWANIE TOPOLOGICZNE    *)
(*        ROZWIĄZANIE: MARCIN ŻOŁEK       *)
(*          RIWJU: PAWEŁ PILARSKI         *)
(******************************************)

exception Cykliczne

let topol lista = 
    let tworz_graf lista = (* utworzenie grafu w postaci mapy list *)
        let pom a h =
            let b = List.fold_left (fun a h -> if PMap.mem h a then a else PMap.add h [] a) a (snd h) in (* mapa po przetworzeniu sąsiadów wierzchołka *)
            if PMap.mem (fst h) b then PMap.add (fst h) ((snd h)@(PMap.find (fst h) b)) b  (* dołączenie sąsiadów wierzchołka do mapy, w której już jest wierzchołek *) 
            else PMap.add (fst h) (snd h) b  (* dołączenie sąsiadów wierzchołka do mapy, w której jeszcze nie ma wierzchołka *) 
        in
        List.fold_left pom PMap.empty lista
    in
    let graf = tworz_graf lista in 
    let tworz_stan lista = (* utworzenie mapy liczb, która reprezentuje stany wierzchołków: 0 - nieprzetworzony; 1 - w trakcie przetwarzania; 2 - przetworzony *)
        let pom a h =
            let b = List.fold_left (fun a h -> PMap.add h 0 a) a (snd h) in (* mapa po przetworzeniu sąsiadów wierzchołka *) 
            PMap.add (fst h) 0 b (* dołączenie wierzchołka do mapy *)
        in
        List.fold_left pom PMap.empty lista
    in
    let rec dfs (stan, porzadek) wierzcholek = (* przeszukiwanie grafu w głąb *)
        let sasiedzi = PMap.find wierzcholek graf in (* lista wierzchołków, do których jest krawędź z wierzchołka *)
        let kolor = PMap.find wierzcholek stan in (* stan wierzchołka *)
        match kolor with
        | 0 -> (* wierzchołek jest odwiedzany po raz pierwszy *) 
            let (stan, porzadek) = List.fold_left (fun a h -> dfs a h) (PMap.add wierzcholek 1 stan, porzadek) sasiedzi in
            (PMap.add wierzcholek 2 stan, wierzcholek::porzadek) 
        | 1 -> raise Cykliczne (* wykryto cykl *)
        | _ -> (stan, porzadek) (* wierzchołek już wcześniej został odwiedzony *)
    in
    let stan = tworz_stan lista in
    let (_, porzadek) = List.fold_left (fun a (h, _) -> if PMap.find h stan = 0 then dfs a h else a) (stan, []) lista in
    porzadek 
