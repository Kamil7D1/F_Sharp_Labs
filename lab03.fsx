open System

type Lista<'a> =
 | Pusta
 | Wezel of 'a*Lista<'a>

let lista = Wezel(1, Wezel(2, Wezel(3, Wezel(4, Pusta))))

let lista2 = Wezel(55, Wezel(23, Wezel(32, Wezel(41, Pusta))))

let glowa = 
    function
    | Pusta -> failwith "Nie monzna pobrac glowy z listy pustej"
    | Wezel(glowa,_) -> glowa

let ogon = 
    function
    | Pusta -> failwith "nie mozna pobrac ogona z listy pustej"
    | Wezel(_,ogon) -> ogon

// let rec liczbaElementow =
//     function
//     | Pusta -> 0
//     | Wezel(_,ogon) -> licz + 1 ogon

let rec dodajPo element nowyElement = 
    function
    | Pusta -> failwith ("Nie znalazlem elementu" + element.ToString())
    | Wezel (glowa, ogon) ->
        if glowa = element then
            Wezel(element, Wezel(nowyElement, ogon))
        else 
            Wezel(glowa, dodajPo element nowyElement ogon)

//let nowaLista = dodajPo 2 4 lista
//printfn "Lista: %A" nowaLista

let zad1 = fun n ->
    let rec loop= fun x  ->
        if x = n then
            Wezel(x, Pusta)
        else
            Wezel(x, loop (x + 1))
    loop 1

//let nowaZad1 = zad1 5
//printfn "Zad1: %A" nowaZad1

let zad2 = fun min max ->
    let rec loop = fun min ->
        if min = max then
            Wezel(min, Pusta)
        else
            Wezel(min, loop(min + 1))
    loop min

//let nowaZad2 = zad2 5 15
//printfn "Zad1: %A" nowaZad2

let zad3 = fun n lista ->
    let rec loop x  =
        function
        | Pusta -> failwith"Nie znaleziono elemetu"
        | Wezel(glowa, ogon) ->
            if x = n then 
                glowa
            else
                loop (x+1) ogon
    loop 1 lista

//printfn "Zad3: %A" (zad3 3 lista2)

let zad4 = fun element lista ->
    let rec loop =
        function
        | Pusta -> failwith"Nie znaleziono elemetu"
        | Wezel(glowa, ogon) ->
            if glowa = element then 
                glowa
            else
                loop ogon
    loop lista

//printfn "Zad4: %A" (zad4 3 lista)

let zad5 = fun element lista ->
    let rec loop index  =
        function
        | Pusta -> failwith"Nie znaleziono elemetu"
        | Wezel(glowa, ogon) ->
            if glowa = element then 
                index
            else
                loop (index + 1) ogon
    loop 1 lista

//printfn "Zad5: %A" (zad5 32 lista2)

let rec zad6 pozycja lista =
    match (pozycja, lista) with
    | (_, Pusta) -> failwith "Lista jest za krótka"
    | (1, Wezel(_, ogon)) -> ogon
    | (_, Wezel(glowa, ogon)) when pozycja > 1 ->
        Wezel(glowa, zad6 (pozycja - 1) ogon)
    | _ -> failwith "Niepoprawna pozycja"

//printfn "Zad6: %A" (zad6 2  lista2)

let zad7 = fun lista ->
    let rec loop = fun suma liczbaElementow ->
        function
        | Pusta -> if liczbaElementow = 0 then 0.0 else float suma / float liczbaElementow
        | Wezel(head, ogon) ->
            loop (suma + head) (liczbaElementow + 1) ogon
    loop 0 0 lista

printfn "zad7 %A" (zad7 lista)