open System


let zad1 = fun (r: float) ->
    if r > 0.0 then
        Math.PI * r**2.0
    else
        0.0

//printfn "Zadanie 1: %.2f" (zad1 2.0)
//printfn "Zadanie 1: %.2f" (zad1 -2.0)

let zad2 = fun (a: float) (b: float) (c: float) ->
    let delta = b**2.0 - 4.0 * a * c

    if delta > 0.0 then
        let x1 = (-b + Math.Sqrt(delta)) / (2.0 * a)
        let x2 = (-b - Math.Sqrt(delta)) / (2.0 * a)
        printfn "Delta: %f. Mamy dwa rozwiązania x1: %f, x2: %f" delta x1 x2
    elif delta = 0.0 then
        let x1 = -b / (2.0 * a)
        printfn "Delta %f. Mamy jedno rozwiązanie x1: %f" delta x1
    else
        printfn "Delta: %f. Delta poniżej zera. Nie ma rozwiązań" delta

//printfn "Zadanie 2: %A" (zad2 1.0 1.0 1.0)
//printfn "Zadanie 2: %A" (zad2 1.0 2.0 1.0)
//printfn "Zadanie 2: %A" (zad2 1.0 4.0 1.0)

let zad3 = fun (a: float) (b: float) (c: float) ->
    if a + b > c && a + c > b && c + b > a then
        true
    else
        false

//printfn "Zadanie 3: %A" (zad3 3.0 5.0 8.0)
//printfn "Zadanie 3: %A" (zad3 4.0 9.0 6.0)

let zad4 = fun (a: float) (b: float) (c: float) ->
    if (zad3 a b c) = true then
        let p = (a + b + c) / 2.0
        let pole = Math.Sqrt(p * (p - a) * (p - b) * (p - c))
        printfn "Pole trójkąta wynosi %.2f" pole
    else    
        failwith "Nie da się zbudować z danych boków trójkąta"
    
//printfn "Zadanie 4: %A" (zad4 3.0 5.0 8.0)
//printfn "Zadanie 4: %A" (zad4 4.0 9.0 6.0)

let rec zad5 = fun (suma: int) (n: int) ->
    if n < 0 then
        failwith "Podana liczba nie może być mniejsza od 0"
    elif n = 0 then 
        suma
    else
        zad5 (suma + n) (n - 1)

//printfn "Zadanie 5: %A" (zad5 0 5)
//printfn "Zadanie 5: %A" (zad5 0 3)    

let rec zad6 = fun (x: int) (n: int) (wynik: int) ->
    if  n = 0 then 
         wynik
    else
        zad6 x (n - 1) (wynik * x)

//printfn "Zadanie 5: %A" (zad6 0 2 1)
//printfn "Zadanie 5: %A" (zad6 2 2 1)

let rec zad7 = fun (n: int) ->
    if n = 0 then
        0
    elif n = 1 then
        1
    else
        zad7(n - 1) + zad7(n - 2)

//printfn "Zadanie 7: %A" (zad7 6)
//printfn "Zadanie 7: %A" (zad7 5)
//printfn "Zadanie 7: %A" (zad7 7)
//printfn "Zadanie 7: %A" (zad7 8)
//printfn "Zadanie 7: %A" (zad7 9)

let rec zad8 = fun (n: int) (k: int) ->
    if k = 0 then
        1
    elif k = n then
        1
    else
        zad8 (n - 1) k + zad8 (n - 1) (k - 1)

//printfn "Zadanie 8: %A" (zad8 10 2) 

let rec zad9 = fun (n: float) (dzielnik: float) ->
    if dzielnik = n then
        zad9 n (dzielnik - 1.0)
    elif n % dzielnik <> 0.0 then
        zad9 n (dzielnik - 1.0)
    elif dzielnik = 1.0 then
        printfn "Liczba jest liczbą pierwszą"
    else
        printfn "Liczba nie jest liczbą pierwszą"

//printfn "Zadanie 9: %A" (zad9 50.0 50.0)

let rec zad10 = fun (rzuty : int) (szostki: int) ->
    let rand = new Random()
    if rzuty = 0 then
        printfn"Procent: %.3f, szostki: %d" (float(szostki) / 1000.0) szostki    
    elif rand.Next(1, 7) = 6 then
        zad10 (rzuty - 1) (szostki + 1)
    else
        zad10 (rzuty - 1) (szostki)
     
//printfn "Zadanie 10: %A" (zad10 1000 0)

let rec zad11 = fun (rzuty: int) (szostki: int) ->
    let rand1 = new Random()
    let rand2 = new Random()
    if rzuty = 0 then
        printfn"Procent: %.3f, szostki: %d" (float(szostki) / 1000.0) szostki    
    elif ((rand1.Next(1, 7) = 6) && (rand2.Next(1, 7) = 6)) then
        zad10 (rzuty - 1) (szostki + 1)
    else
        zad10 (rzuty - 1) (szostki)
     
//printfn "Zadanie 11: %A" (zad11 1000 0)

let zad12 = fun (n: float) (m: float) ->
    let dzielnik = if n > m then m else n

    let rec loop (n: float) (m: float) (dzielnik: float) =
        if (n % dzielnik = 0.0 && m % dzielnik = 0.0) then
            dzielnik
        else
            loop n m (dzielnik - 1.0)

    loop n m dzielnik

//printfn "Zadanie 12: %.0f" (zad12 282.0 78.0)
//printfn "Zadanie 12: %.0f" (zad12 1232310.0 9233310.0)

// ZAD 13
// ZAD 14

// JEDNOSTKI MIARY

// ---------------

let zad17 = fun (palindrom: string) ->
    let cleanedPalindrome = palindrom.Replace(" ", "")
    let length = cleanedPalindrome.Length - 1
    
    let rec loop length i =
        if i >= length then
            printfn "Podane słowo jest palindromem"
        elif cleanedPalindrome.[i] = cleanedPalindrome.[length] then
            loop (length  - 1) (i + 1)    
        else
            printfn "Podane słowo nie jest palindromem"

    loop length 0

//printfn "Zadanie 17: %A" (zad17 "kajak") 
//printfn "Zadanie 17: %A" (zad17 "kobyła ma mały bok")
//printfn "Zadanie 17: %A" (zad17 "mów dała dwóm")  
//printfn "Zadanie 17: %A" (zad17 "elo")
//printfn "Zadanie 17: %A" (zad17 "siema")

let zad18 = fun (slowo: string) (znak: char) ->

    let rec loop i count =
        if i < slowo.Length then
            if slowo.[i] = znak then
                loop (i + 1) (count + 1)
            else
                loop (i + 1) (count)
        else
            count    
    loop 0 0 

//printfn "Zadanie 18: Liczba wystąpień %A" (zad18 "siema siema siema siema" 'a')

let zad19 = fun (tekst: string) ->
    if tekst = " " then
        printfn "Pusty tekst"

    let rec loop i count =
        if i < tekst.Length then
            if tekst.[i] = ' ' then
                loop (i + 1) (count + 1)
            else 
                loop (i + 1) (count)
        else
            count + 1
    loop 0 0

//printfn "Zadanie 19: %A" (zad19 "Siema siema siema siema siema")

let zad20 = fun (tekst: string) ->
    let rec loop count i =
        if i = (tekst.Length - 1) then
            count
        else  
            if Char.IsDigit(tekst.[i]) && Char.IsDigit(tekst.[i + 1]) then
                loop (count + 1) (i + 1)
            else
                loop (count) (i + 1)
    loop 0 0 

//printfn "Zadanie 20: %A" (zad20 "Siema 121212 siema 1212 siema 12")

let zad21 = fun (imieNazwisko: string) ->
    printfn "Witaj %s" imieNazwisko

//zad21 "Kamil Czudaj"

let zad22 = fun (rok: int) ->
    if (rok % 4 = 0) then
        printf "Podany rok jest przestępny"
    else 
        printf "Podany rok nie jest przestępny"

//zad22(2024)

let zad23 = fun (a: float) (b: float) (c: float) ->
    if(a + b > c && a + c > b && c + a > b) then
        printfn "Z podanych boków można utworzyć trójkąt:"
        if a = b && b = c  then
            printfn "-równoboczny"
        if ((a = b) && (a + b > c)) || ((b = c) && (b + c > a)) || ((c = a) && (c + a > b))then 
            printfn "-równoramienny"
        if (a**2.0 = b**2.0 + c**2.0) || (c**2.0 = a**2.0 + b**2.0) || (b**2.0 = a**2.0 + c**2.0) then  
            printfn "-prostokątny" 
    else
        printfn "Z podanuch boków nie można utworzyć trójkąt."


//zad23 3.0 4.0 5.0
//zad23 1.0 1.0 1.0
//zad23 1.0 1.0 -1.0

let zad24 = fun () ->
    printfn "Podaj swój pesel"
    let pesel = Console.ReadLine()


    if pesel.Length <> 11  then
        printfn "PESEL: %s, jest nie poprawny." pesel
    
    let rok = pesel.Substring(0, 2)
    let miesiac = pesel.Substring(2, 2)
    let dzien= pesel.Substring(4, 2)
    let plec = " "

    let plec =
        if int(pesel.Substring(9, 1)) % 2 = 0 then 
            "kobieta"
        else    
            "mezczyzna"

    printfn "Osoba została urodzona w %s.%s.%s i jest %s" dzien miesiac rok plec

//zad24();

let zad25 = fun () ->
    printfn "Podaj tekst do zaszyfrowania i klucz.\nPodaj tekst:"
    let tekst = Console.ReadLine()
    printfn "\nPodaj klucz:"
    let kluczStr = Console.ReadLine()
    let klucz = int kluczStr

    let alfabet = "abcdefghijklmnopqrstuvwxyz"

    let rec loop i tekstZaszyfrowany =
        if i < tekst.Length then
            let znak = tekst.[i]
            if znak = ' ' then
                loop (i + 1) (tekstZaszyfrowany + " ")
            else
                loop (i + 1) (tekstZaszyfrowany + string(alfabet.[(alfabet.IndexOf(Char.ToLower znak) + klucz) % 26]))
        else
            printfn "TEKST: %s" tekstZaszyfrowany

    loop 0 ""

//zad25()

let zad26 = fun () ->
    printfn "Podaj tekst do odszyforwania i klucz.\nPodaj tekst:"
    let tekst = Console.ReadLine()
    printfn "\nPodaj klucz:"
    let kluczStr = Console.ReadLine()
    let klucz = int kluczStr

    let alfabet = "abcdefghijklmnopqrstuvwxyz"

    let rec loop i tekstOdkodowany =
        if i < tekst.Length then
            let znak = tekst.[i]
            if znak = ' ' then
                loop (i + 1) (tekstOdkodowany + " ")
            else
                loop (i + 1) (tekstOdkodowany + string(alfabet.[(alfabet.IndexOf(Char.ToLower znak) - klucz + 26) % 26]))
        else
            printfn "TEKST: %s" tekstOdkodowany

    loop 0 ""

//zad26()

let zad27 = fun () ->
    printfn "Podaj minuty:"
    let min = Console.ReadLine()
    let minInt = int(min)

    if minInt > (24*60) then
        printfn "Podałeś za duzo minut"

    let godzina = minInt / 60;
    let minuty = minInt % 60;

    printfn "Jest %s godziny i %s minuty po północy" (string(godzina)) (string(minuty))

//zad27()
 
