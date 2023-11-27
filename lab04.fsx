open System

let lista = [1; 2; 3; 4; 5]

let zad1 = fun (zmiana) (lista) ->
    List.map(zmiana) lista

printfn "Zadanie1: %A" (zad1 (fun x -> x *x) lista)


let random = Random()
let liczbaLosowa () = random.Next(-2, 2)

let listaLosowychLiczb = List.init 10 (fun _ -> liczbaLosowa())
printfn "Lista losowych liczba: %A" listaLosowychLiczb

let zad6 (lista) =
    ([List.filter(fun x -> x >= 0) lista],[List.filter(fun x -> x < 0) lista])

printfn "Zadanie6: %A" (zad6 listaLosowychLiczb)

let zad7 (lista: List<int>) =
    let suma = List.sum lista
    let liczbaElementow = List.length lista
    let srednia = suma / liczbaElementow
    ([List.filter(fun x -> x < srednia) lista], [List.filter(fun x -> x > srednia) lista])

printfn "Zadanie7: %A" (zad7 lista)

let listaLosowychLiczb1000 = List.init 1000 (fun _ -> liczbaLosowa())

let zad14 = fun (lista: List<int>) (mapa: Map<int, int>) ->
    let rec loop = fun (lista: List<int>)(mapa: Map<int, int>) ->
        match lista with
        | [] -> mapa
        | head::tail -> 
            if mapa.ContainsKey head then 
                loop tail  (Map.add head (mapa.[head] + 1) mapa)
            else 
                loop tail (Map.add head 1 mapa)       
        
    loop lista mapa

printfn "Zad14: %A" (zad14 listaLosowychLiczb1000 Map.empty<int, int>)

type Drzewo<'a> =
    | Puste
    | Wezel of 'a * Drzewo<'a> * Drzewo<'a>

let rec agregujDrzewo (drzewo: Drzewo<'a>) (poczatkowaWartosc: 'b) (funkcjaAgregacji) =
    match drzewo with
    | Puste -> poczatkowaWartosc
    | Wezel(value, left, right) ->
        let wynikLewy = agregujDrzewo left poczatkowaWartosc funkcjaAgregacji
        let wynikPrawy = agregujDrzewo right poczatkowaWartosc funkcjaAgregacji
        funkcjaAgregacji value wynikLewy wynikPrawy

let drzewo = Wezel(1, Wezel(2, Puste, Puste), Wezel(3, Puste, Puste))

let sumaWartosci = agregujDrzewo drzewo 0 (fun x left right -> x + left + right)

//printfn "Suma wartości w drzewie: %d" sumaWartosci

let rec czyWartoscSpelniaWarunek (drzewo: Drzewo<'a>) (warunek: 'a -> bool) : bool =
    match drzewo with
    | Puste -> false
    | Wezel(value, left, right) ->
        if warunek value then
            true
        else
            czyWartoscSpelniaWarunek left warunek || czyWartoscSpelniaWarunek right warunek

let warunek = fun x -> x = 2

let wynik = czyWartoscSpelniaWarunek drzewo warunek

//printfn "Czy drzewo zawiera wartość spełniającą warunek? %b" wynik

let rec budujDrzewo (lista: 'a list) : Drzewo<'a> =
    match lista with
    | [] -> Puste
    | x :: xs ->
        let mniejsze = List.filter (fun el -> el < x) xs
        let wieksze = List.filter (fun el -> el >= x) xs
        Wezel(x, budujDrzewo mniejsze, budujDrzewo wieksze)

let listaLiczb = [4; 2; 6; 1; 3; 5; 7]
//let mojeDrzewo = budujDrzewo listaLiczb
