open System

let zad1 = fun () ->
    printfn "Podaj wartość: "
    let wartosc = Console.ReadLine()
    let wartoscInt = int(wartosc)

    match wartoscInt with
    | 1 -> printfn "Wprowadziłeś 1"
    | 2 -> printfn "Wprowadziłeś 2"
    | 3 -> printfn "Wprowadziłeś 3"
    | 4 -> printfn "Wprowadziłeś 4"
    | _ -> printfn "Wprowadzileś inną wartośc niż 1,2,3 lub 4"

//zad1()

let zad2 = fun() ->
    printfn "Podaj dwie wartości: "
    let a = Console.ReadLine()
    let b = Console.ReadLine()
    
    let para = (int a, int b)

    match para with
    | (a, b) when a > b -> printfn "Piewsza liczba jest większa: %d" a
    | (a, b) when a < b -> printfn "Druga liczba jest większa: %d" b
    | _ -> printf "Podane liczby są równe "

//zad2()

let zad3 = fun (a: float) (b: float) (c: float) -> ( a+b+c,  sqrt(((a+b+c)/2.0)*(((a+b+c)/2.0) - a)*(((a+b+c)/2.0) - b)*(((a+b+c)/2.0) - c)))
let obwodIPole = zad3 2.0 2.0 2.0
//printfn "Obwód trójkąta to %f, a pole %f" (fst obwodIPole) (snd obwodIPole)

let zad4 = fun (email: string) ->
    let emailSplited = email.Split('@')
    let para = (emailSplited.[0], emailSplited.[1])
    para

//zad4 "kamil@wp.pl"

let zad5 = fun (email: string) ->
    let para = zad4 email

    match para with
    | (nazwa, domena) when domena = "pcz.pl" -> printfn "Email użytkownika %s należy do domeny PCz" nazwa
    | (nazwa, _) -> printfn "Email użytkownika %s nie należy do doemny PCz" nazwa

//zad5 "kamil@wp.pl"
//zad5 "kamil@pcz.pl"


let punkt1 = (2.0, 3.0, 4.0)
let punkt2 = (6.0, 7.0, 10.0)

let zad6 = fun (a1, a2, a3)  (b1, b2, b3) ->
    Math.Sqrt(((a1 - b1)*(a1 - b1)) + ((a2 - b2)*(a2 - b2)) + ((a3 - b3)*(a3 - b3)))

//printfn "Odleglosc euklidesowa pomiedzy dwoma punktami w przestrzeni 3D %A" (zad6 punkt1 punkt2)

let zad7 = fun (srd) (r) (pnkt) ->
    let srdPnkt = Math.Sqrt((fst srd - fst pnkt) * (fst srd - fst pnkt) + (snd srd - snd pnkt) * (snd srd - snd pnkt))
    
    match (srdPnkt, r) with
    | srdPnkt, r when srdPnkt > r -> printfn "Punkt poza okregiem. Srednica: %f i Odleglosc %f" r srdPnkt
    | srdPnkt, r when srdPnkt < r -> printfn "Punkt w okregu. Srednica: %f i Odleglosc %f" r srdPnkt
    | _ -> printfn "Punkt na krawedzi"

//printfn "zad7 %A" (zad7 (0.0, 0.0) (5.0) (1.0, 1.0))
//printfn "zad7 %A" (zad7 (0.0, 0.0) (5.0) (4.0, 4.0))
//printfn "zad7 %A" (zad7 (0.0, 0.0) (5.0) (5.0, 5.0))

type Ulamek = {
    licznik: float
    mianownik: float
}

//zad8 do powtorki

let zad9 = fun (u1: Ulamek) (u2: Ulamek) (operacja: string) ->
    match operacja with
    | "dodawanie" -> printfn "Wynik dodawana ulamkow to: %f" ((u1.licznik + u2.licznik) / (u1.mianownik * u2.mianownik))
    | "odejmowanie" -> printfn "Wynik odejmowania ulamkow to: %f" (((u1.licznik * u2.mianownik) - (u2.licznik * u1.mianownik)) / (u1.mianownik * u2.mianownik))
    | "mnozenie" -> printfn "Wynik mnozenia ulamkow to: %f" ((u1.licznik * u2.licznik) / (u1.mianownik * u2.mianownik))
    | "dzielenie" -> printfn "Wynik dzielenia ulamkow to: %f" ((u1.licznik * u2.mianownik) / (u1.mianownik * u2.licznik))
    | _ -> printfn "Podana operacja nie istenieje"

//zad9 ({licznik = 2.0; mianownik = 2.0}) ({licznik = 2.0; mianownik = 2.0}) "dodawanie"
//zad9 ({licznik = 2.0; mianownik = 2.0}) ({licznik = 2.0; mianownik = 2.0}) "odejmowanie"
//zad9 ({licznik = 2.0; mianownik = 2.0}) ({licznik = 2.0; mianownik = 2.0}) "mnozenie"
//zad9 ({licznik = 2.0; mianownik = 2.0}) ({licznik = 2.0; mianownik = 2.0}) "dzielenie"

type DzienTygodnia =
    | Poniedzialek
    | Wtorek
    | Sroda
    | Czwartek
    | Piatek
    | Sobota
    | Niedziela

type Data = { Dzien: int; Miesiac: int; Rok: int }
// zad10

let zad11 = fun (dzielna: float) (dzielnik: float) ->
    if dzielnik = 0.0 then
        0.0, false
    else
        dzielna / dzielnik, true

//printfn "Wynik dzielenia %A" (zad11 2.0 0.0)
//printfn "Wynik dzielenia %A" (zad11 2.0 2.0)

//zad12

let zad13 = fun (a: float) (b: float) (c: float) ->
    if (a + b > c && a + c > b && c + a > b) then
        let p = (a + b + c) / 2.0
        let obwodPole = (a + b + c, Math.Sqrt(p * (p - a) * (p - b) * (p - c)))
        printfn "Obwod trojkata to: %f ,a pole to: %f" (fst obwodPole) (snd obwodPole)
    else
        printfn "Z podanych wartosci nie da sie stowrzyc trojkata"

//zad13 2.0 2.0 2.0

type Wynik = 
    | DwaRozwiazania of x1:float*x2:float
    | JednoRozwiazanie of x1:float
    | ZeroRozwiazan

//zad14 zad15 zad16