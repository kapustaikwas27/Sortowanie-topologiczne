(******************************************)
(*     ZADANIE SORTOWANIE TOPOLOGICZNE    *)
(*        ROZWIĄZANIE: MARCIN ŻOŁEK       *)
(*          RIWJU: PAWEŁ PILARSKI         *)
(******************************************)

exception Cykliczne

let topol lista = 
    let tworz_graf lista = (* utworzenie grafu w postaci mapy list *)
        List.fold_left (fun a1 h1 -> let b = List.fold_left (fun a2 h2 -> if PMap.mem h2 a2 then a2 else PMap.add h2 [] a2) a1 (snd h1) in if PMap.mem (fst h1) b then PMap.add (fst h1) ((snd h1)@(PMap.find (fst h1) b)) b else PMap.add (fst h1) (snd h1) b) PMap.empty lista
    in
    let tworz_stan lista = (* utworzenie mapy liczb, która reprezentuje stany wierzchołków: 0 - nieprzetworzony; 1 - w trakcie przetwarzania; 2 - przetworzony *)
        List.fold_left (fun a1 h1 -> PMap.add (fst h1) 0 (List.fold_left (fun a2 h2 -> PMap.add h2 0 a2) a1 (snd h1))) PMap.empty lista
    in
    let rec dfs (graf, stan, porzadek) wierzcholek = (* przeszukiwanie grafu w głąb *)
        let sasiedzi = PMap.find wierzcholek graf in (* lista wierzchołków, do których jest krawędź z wierzchołka *)
        let kolor = PMap.find wierzcholek stan in (* stan wierzchołka *)
        match kolor with
        | 0 -> iteruj (graf, PMap.add wierzcholek 1 stan, porzadek) wierzcholek sasiedzi (* wierzchołek jest odwiedzany po raz pierwszy *) 
        | 1 -> raise Cykliczne (* wykryto cykl *)
        | _ -> (graf, stan, porzadek) (* wierzchołek już wcześniej został odwiedzony *)
    and iteruj (graf, stan, porzadek) wierzcholek sasiedzi =
        match sasiedzi with
        | [] -> (graf, PMap.add wierzcholek 2 stan, wierzcholek::porzadek)
        | h::t -> iteruj (dfs (graf, stan, porzadek) h) wierzcholek t
    in
    let graf = tworz_graf lista in 
    let stan = tworz_stan lista in
    let (_, _, porzadek) = List.fold_left (fun a (h, _) -> if PMap.find h stan = 0 then dfs a h else a) (graf, stan, []) lista in
    porzadek 
