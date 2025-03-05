//На основе списка вещественных чисел получить список из их последних цифр. 
//Подсказка. Преобразовать число в строку, выделить последний символ, 
//преобразовать его обратно в число.

open System
open System.IO

// Парсинг с выводом ошибок
let parseFloat s =
    try
        Some (float s)
    with
    | :? System.FormatException ->
        printfn "'%s' не допустимый формат числа" s
        None
    | ex ->
        printfn "ошибка: %s" ex.Message
        None

// Получение списка float с клавиатуры
let сonsoleReadNums mess =
    Console.Clear()
    printf "%s >> " mess
    let line = Console.ReadLine()

    if line = "" then
        Some []
    else
        let nums =
            // получение списка чисел из строки
            line.Split(' ')
            |> List.ofArray
            |> List.map parseFloat

        // отработка ошибки при парсинге
        // список без None станет содержать просто float
        if (List.contains None nums) then
            None
        else
            Some (List.choose id nums)


let readFromFile filePath =
    Console.Clear()
    try
        let lines = File.ReadAllLines(filePath)
        if lines = [||] then
            Some []
        else
            let nums = 
                List.ofArray lines
                |> List.map parseFloat

            // отработка ошибки при парсинге
            // список без None станет содержать просто float
            if (List.contains None nums) then
                None
            else
                Some (List.choose id nums)  
    with
    | :? FileNotFoundException -> failwith "Файл не найден"
    | :? IOException as ex -> failwith (sprintf "Ошибка ввода-вывода: %s" ex.Message)
    

// Ввод числа int >= 0
let rec readSize mess =
    printf "%s >> " mess
    let s = Console.ReadLine()
    try
        let num = int s
        if (num >= 0) && (num <= 10000) then
            Some num
        else
            printfn "число '%s' не допустимый размер, возможные: 0 1 2 3 4 ... 10000" s
            None
    with
        | :? System.FormatException ->
            printfn "'%s' не допустимый формат числа" s
            None
        | :? System.OverflowException ->
            printfn "число '%s' слишком большое" s
            None
        | ex ->
            printfn "ошибка: %s" ex.Message
            None

let getRandFloat (maxVal, minVal) =
    minVal + Random().NextDouble() * (maxVal - minVal)

// Генерация списка случайных float чисел
let rec getRandFloats n = 
    if n <= 0 then
        []
    else
        getRandFloat (-50,50) :: getRandFloats (n-1)

let randomGen () =
    Console.Clear()
    let num =  readSize "Введите кол-во случайных чисел в списке"
    if num <> None then
        Some (getRandFloats (Option.get num))
    else
        None

let rec getNums mess =
    Console.Clear()
    printfn "%s" mess
    printfn "   [1] Ввести с клавиатуры"
    printfn "   [2] Сгенерировать случайно"
    printfn "   [3] Из файла"
    printfn "   [Esc] <- назад"

    match Console.ReadKey(true).Key with
    | ConsoleKey.D1 -> сonsoleReadNums "введите список чисел через пробел"
    | ConsoleKey.D2 -> randomGen ()
    | ConsoleKey.D3 -> readFromFile "test.txt"
    | ConsoleKey.Escape -> None
    | _ -> getNums (mess)

let getLastDigit num =
    let s = string num
    if s.Contains('E') then
        // экспоненциальное
        if s.Contains("E-") then
            // порядок отрицательный
            Some (int (string s.[s.IndexOf('E') - 1]))
        else
            // порядок положительный
            Some 0
    elif s.Contains("Infinity") then
        // бесконечность
        None
    else
        // скучно десятичное
        Some (int (string s.[s.Length-1]))


let rec main () =
    let newLst = getNums ("Выберете метод получения списка вещественных")
    if newLst <> None then
        let listOfNums = Option.get newLst

        Console.Clear()
        printfn "Получен список %A" (List.map string listOfNums)

        let lastDigits = List.map getLastDigit listOfNums
        if (List.contains None lastDigits) then
            printfn "Числа в списке слишком большие, выделение последних цифр невозможно"
        else
            printfn "Последнее цифры: %A" (List.map Option.get lastDigits)

    // выход
    printf        "Любую клавишу для повторного ввода списка, для выхода Esc..."
    if (Console.ReadKey(true).Key <> ConsoleKey.Escape) then
        printfn "\r                                                            "
        main ()
    else
        printfn "\rПрограмма приостановлена                                    "


// =========== ТОЧКА ВХОДА ===========
main ()
