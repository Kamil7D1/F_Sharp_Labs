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

//printfn "zad7 %A" (zad7 lista)

let zad8 = fun (lista: Lista<string>) (separator: string) ->
    let rec loop = fun nowyString -> 
        function
        | Pusta -> nowyString
        | Wezel(head, ogon) ->
            if ogon = Pusta then
                loop (nowyString + head) ogon
            else
                loop (nowyString + head + separator) ogon
    loop "" lista

let tablicaString = Wezel("Ala", Wezel("ma", Wezel("kota", Pusta)))

//printfn "Zad8: %A" (zad8 tablicaString " ")

let zad9 = fun (lista: Lista<string>) (pozycja: int) ->
    let rec loop = fun (n: int) ->
        function
        | Pusta -> failwith "Nie znaleziono danej pozycji"
        | Wezel(glowa, ogon) ->
            if n = pozycja then
                String.length(glowa)
            else
                loop (n+1) ogon
    loop 1 lista

//printfn "Zad8: %A" (zad9 tablicaString 3)

let zad10 = fun (lista: Lista<string>) ->
    let rec loop = fun (min: string) (max: string) ->
        function
        | Pusta -> (min, max)
        | Wezel(glowa, ogon) ->
            if String.length(glowa) > String.length(max) then
                loop min (glowa) ogon
            elif String.length(glowa) < String.length(min) then
                loop (glowa) (max) ogon
            else
                loop (min) (max) ogon
    match lista with
    | Pusta -> ( "", "" )  // Obsługa przypadku pustej listy
    | Wezel (pierwszyElement, resztaListy) ->
        loop pierwszyElement pierwszyElement resztaListy

let listaStringow = Wezel("Ala", Wezel("ma", Wezel("kota", Wezel("Kamil", Wezel("K", Pusta)))))

//printfn "Lista10: %A" (zad10 listaStringow)

// inne rozwiazanie
let zad10v2 (lista: Lista<string>) =
    let rec loop min max = function
        | Pusta -> (min, max)
        | Wezel (glowa, ogon) ->
            let dlugoscGlowy = String.length glowa
            let nowyMin = if dlugoscGlowy < String.length min then glowa else min
            let nowyMax = if dlugoscGlowy > String.length max then glowa else max
            loop nowyMin nowyMax ogon

    match lista with
    | Pusta -> failwith "Lista nie może być pusta."
    | Wezel (pierwszyElement, resztaListy) ->
        loop pierwszyElement pierwszyElement resztaListy
        
let zad11 (listaImion: Lista<string>) (nowaLista: Lista<string>) =
    // Funkcja pomocnicza, która przechodzi przez listę
    let rec loop (lista: Lista<string>)  (nowaLista: Lista<string>) =
        match lista with
        | Pusta -> nowaLista
        | Wezel(glowa, ogon) ->
            // Sprawdź, czy ostatnia litera imienia to 'a'
            if glowa.[glowa.Length - 1] = 'a' then
                // Dodaj imię do nowej listy
                loop ogon (Wezel(glowa, nowaLista))
            else
                // Pomijaj imię i kontynuuj przetwarzanie
                loop ogon nowaLista

    // Wywołaj funkcję pomocniczą z początkowymi danymi
    loop listaImion Pusta

let imiona = Wezel("Kamil", Wezel("Ania", Wezel("Kasia", Wezel("Asia", Wezel("Mateusz", Pusta)))));
let nowaLista = Pusta

printfn "Zadanie 11: %A" (zad11 imiona nowaLista)

let rec zad12 (lista: Lista<'a>) : Lista<'a> =
    let rec pomocnicza (wejscie: Lista<'a>) (wyjscie: Lista<'a>) =
        match wejscie with
        | Pusta -> wyjscie
        | Wezel(glowa, ogon) -> pomocnicza ogon (Wezel(glowa, wyjscie))
    
    pomocnicza lista Pusta

let rec zad13 (listaImion: Lista<string>) (listaZ: Lista<string>) (listaM: Lista<string>) = 
    match listaImion with
    | Pusta -> (listaZ, listaM)
    | Wezel(head, tail) ->
    if head.[head.Length - 1] = 'a' then
        zad13 tail (Wezel(head, listaZ)) listaM
    else
        zad13 tail listaZ (Wezel(head, listaM))

//printfn "zad13: %A" (zad13 imiona Pusta Pusta)

let rec zad14 (lista1: Lista<int>) (lista2: Lista<int>) : Lista<bool> =
    match lista1, lista2 with
    | Pusta, Pusta -> Pusta
    | _, Pusta | Pusta, _ -> failwith "Listy muszą być tej samej długości"
    | Wezel(glowa1, ogon1), Wezel(glowa2, ogon2) ->
        let wiekszaNaPierwszejLiscie = glowa1 > glowa2
        Wezel(wiekszaNaPierwszejLiscie, zad14 ogon1 ogon2)

// Zdefiniuj typ wyliczeniowy do reprezentacji wyników porównania
type WynikPorownania =
    | Pierwsza
    | Druga

let rec zad15 (lista1: Lista<int>) (lista2: Lista<int>) : Lista<WynikPorownania> =
    match lista1, lista2 with
    | Pusta, Pusta -> Pusta
    | _, Pusta -> Wezel(Pierwsza, zad15 Pusta lista1)
    | Pusta, _ -> Wezel(Druga, zad15 Pusta lista2)
    | Wezel(glowa1, ogon1), Wezel(glowa2, ogon2) ->
        if glowa1 > glowa2 then
            Wezel(Pierwsza, zad15 ogon1 lista2)
        else if glowa1 < glowa2 then
            Wezel(Druga, zad15 lista1 ogon2)
        else
            Wezel(Pierwsza, zad15 ogon1 ogon2) 

type KierunekSortowania =
    | Rosnaco
    | Malejaco

let rec zad16 (lista: Lista<'a>) (kierunek: KierunekSortowania) : bool =
    match lista with
    | Pusta | Wezel(_, Pusta) -> true
    | Wezel(glowa1, Wezel(glowa2, ogon)) ->
        match kierunek with
        | Rosnaco -> glowa1 <= glowa2 && zad16 (Wezel(glowa2, ogon)) Rosnaco
        | Malejaco -> glowa1 >= glowa2 && zad16 (Wezel(glowa2, ogon)) Malejaco

let rec zad17 (lista1: Lista<int>) (lista2: Lista<int>) : Lista<int> =
    match lista1, lista2 with
    | Pusta, _ -> lista2
    | _, Pusta -> lista1
    | Wezel(glowa1, ogon1), Wezel(glowa2, ogon2) ->
        if glowa1 <= glowa2 then
            Wezel(glowa1, zad17 ogon1 lista2)
        else
            Wezel(glowa2, zad17 lista1 ogon2)

// Zdefiniuj typ elementu stosu
type Stos<'a> =
    | Pusty
    | Element of 'a * Stos<'a>

// Funkcja dodająca element na stos
let push (element: 'a) (stos: Stos<'a>) : Stos<'a> =
    Element(element, stos)

// Funkcja usuwająca element ze stosu
let pop (stos: Stos<'a>) : 'a * Stos<'a> =
    match stos with
    | Pusty -> failwith "Stos jest pusty"
    | Element(glowa, ogon) -> glowa, ogon

// Funkcja sprawdzająca, czy stos jest pusty
let isEmpty (stos: Stos<'a>) : bool =
    match stos with
    | Pusty -> true
    | _ -> false

let stosPusty = Pusty
let stos1 = push 1 stosPusty
let stos2 = push 2 stos1

let (element, nowyStos) = pop stos2
//printfn "Zdjety element: %d" element

//let czyPusty = isEmpty nowyStos
//printfn "Stos jest pusty: %b" czyPusty

// DRZWA BINARNE

type Drzewo =
    | Puste
    | Wezel of float * Drzewo * Drzewo

let drzewo = Wezel(4.0, Wezel(2.0, Wezel(1.0, Puste, Puste), Wezel(3.0, Puste, Puste)), Wezel(7.0, Puste, Wezel(8.0, Puste, Puste)))

let rec zad20 (tree: Drzewo) : int =
    match tree with
    | Puste -> 0
    | Wezel(_, left, right) -> 1 + zad20 left + zad20 right

printfn "Tyle jest elementow w drzewie: %A" (zad20 drzewo)