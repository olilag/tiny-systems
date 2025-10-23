// ----------------------------------------------------------------------------
// 02 - Implement interactive program editing
// ----------------------------------------------------------------------------
module TinyBASIC

type Value = StringValue of string

type Expression = Const of Value

type Command =
    | Print of Expression
    | Run
    | Goto of int

type State = { Program: list<int * Command> }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value =
    match value with
    | StringValue s -> printfn "%s" s

let getLine state line =
    let findFn = fun (l, _) -> l = line
    let res = List.tryFind findFn state.Program

    match res with
    | Some v -> v
    | None -> failwithf "no line with number: %d" line

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evalExpression expr =
    match expr with
    | Const v -> v

let rec runCommand state (line, cmd) =
    match cmd with
    | Print(expr) ->
        let v = evalExpression expr
        printValue v
        runNextLine state line
    | Run ->
        let first = List.head state.Program
        runCommand state first
    | Goto(line) ->
        let goto = getLine state line
        runCommand state goto

and runNextLine state line =
    let findFn = fun (l, _) -> l > line
    let res = List.tryFind findFn state.Program

    match res with
    | Some v -> runCommand state v
    | None -> state

// ----------------------------------------------------------------------------
// Interactive program editing
// ----------------------------------------------------------------------------

let runInput state (line, cmd) =
    match line with
    | Some n ->
        let indexFn = fun (l, _) -> l >= n
        let idx = List.tryFindIndex indexFn state.Program

        let i =
            match idx with
            | Some i -> i
            | None -> state.Program.Length

        let l = List.insertAt i (n, cmd) state.Program
        { state with Program = l }
    | None -> runCommand state (-1, cmd)

let runInputs state cmds =
    let foldFn = fun acc elem -> runInput acc elem
    List.fold foldFn state cmds

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

let helloOnce =
    [ Some 10, Print(Const(StringValue "HELLO WORLD\n"))
      Some 10, Print(Const(StringValue "HELLO NPRG077\n"))
      None, Run ]

let helloInf =
    [ Some 20, Goto 10
      Some 10, Print(Const(StringValue "HELLO WORLD\n"))
      Some 10, Print(Const(StringValue "HELLO NPRG077\n"))
      None, Run ]

let empty = { Program = [] }

runInputs empty helloOnce |> ignore
runInputs empty helloInf |> ignore
