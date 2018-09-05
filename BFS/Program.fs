open System
open System.IO

let bf_extended_chars = ['+';'-';'[';']';'<';'>';'.';',';'@';'$';'#';'"';'"';'{';'}';'|';'/';'*';';';':';'\\';'?';'%']

let bf_allowed = bf_extended_chars.[0..7]

let filter_program arr allowed = List.filter (fun c -> (List.exists (fun x -> x = c) allowed)) (arr |> Seq.toList)

let check_brackets arr =
    let rec cb_rec a s =
        match a with
        | [] ->
            if (List.length s) = 0 then true
            else false
        | _ ->
            match List.head a with
            | '[' -> cb_rec (List.tail a) ('1'::s)
            | ']' ->
                if (List.length s <> 0) then
                    cb_rec (List.tail a) (List.tail s)
                else false
            | _ -> cb_rec (List.tail a) s
    cb_rec arr []

let mutable stack = Array.zeroCreate<uint8> 20000
let mutable counter = 0
let mutable register = 0uy
let mutable brackets:int list = []
let mutable instruction_ptr = 0

let run input chars=
    let arr = filter_program input chars
    let rec jump c =
        match arr.[c] with
        | ']' ->
            let h = brackets.Head
            brackets <- brackets.Tail
            if h = instruction_ptr then instruction_ptr <- c
            else jump (c+1)
        | '[' ->
            brackets <- c::brackets
            jump (c+1)
        | _ -> jump (c+1)

    let rec run_rec () =
        if arr.Length <= instruction_ptr then ()
        else
            match arr.[instruction_ptr] with
            | '+' -> stack.[counter] <- stack.[counter] + 1uy
            | '-' -> stack.[counter] <- match stack.[counter] - 1uy with | n when n>= 0uy -> n | _ -> 0uy
            | '>' -> counter <- counter + 1
            | '<' -> counter <- match counter - 1 with | n when n >= 0 -> n | _ -> 0
            | '.' -> printf "%c" (stack.[counter] |> char)
            | ',' ->
                printf "\nSet value in %A: " counter
                stack.[counter] <- Convert.ToByte(Console.ReadLine() |> char)
            | '@' -> printfn "\nStack position: %A" counter
            | '$' -> printfn "\nStack value: %A" stack.[counter]
            | '|' -> register <- stack.[counter]
            | ';' -> stack.[counter] <- stack.[counter] + register
            | ':' -> stack.[counter] <- stack.[counter] - register
            | '*' -> stack.[counter] <- stack.[counter] * register
            | '/' -> stack.[counter] <- stack.[counter] / register
            | '[' ->
                    brackets <- instruction_ptr::brackets
                    if stack.[counter] = 0uy then jump (instruction_ptr + 1)
            | ']' ->
                    if stack.[counter] <> 0uy then
                        instruction_ptr <- brackets.Head
                    else
                        brackets <- brackets.Tail
            | '%' -> printfn "Register value: %A" register
            | _ -> ()
            instruction_ptr <- instruction_ptr + 1
            run_rec ()
    if (check_brackets arr) then
        run_rec ()
    else
        printfn "Invalid Program"

let rec main_rec chars =
    printf "\nBF F# I $ "
    let input = Console.ReadLine()
    if input.Trim() = "quit" then 0
    else
        run input chars
        main_rec chars

[<EntryPoint>]
let main argv =
    let files = Array.filter (fun (x:String) -> x.Contains(".bf")) argv
    let chars = if Array.exists (fun x -> x = "-bf") argv then bf_allowed else bf_extended_chars
    if Array.length files <> 0 then
        let prog = Array.map (fun x -> File.ReadAllLines x |> String.concat "") files |> String.concat ""
        run prog chars
        0
    else
        main_rec chars
