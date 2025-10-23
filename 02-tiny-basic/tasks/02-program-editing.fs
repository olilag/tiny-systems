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
    // TODO: Find a program line with the number greater than 'line' and evalaute
    // it using 'runCommand' (if found) or just return 'state' (if not found).
    let findFn = fun (l, _) -> l > line
    let res = List.tryFind findFn state.Program

    match res with
    | Some v -> runCommand state v
    | None -> state

// ----------------------------------------------------------------------------
// Interactive program editing
// ----------------------------------------------------------------------------

let runInput state (line, cmd) =
    // TODO: Simulate what happens when the user enters a line of code in the
    // interactive terminal. If the 'line' number is 'Some ln', we want to
    // insert the line into the right location of the program (addLine); if it
    // is 'None', then we want to run it immediately. To make sure that
    // 'runCommand' does not try to run anything afterwards, you can pass
    // 'System.Int32.MaxValue' as the line number to it (or you could use -1
    // and handle that case specially in 'runNextLine')

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
    // TODO: Apply all the specified commands to the program state using 'runInput'.
    // This is a one-liner if you use 'List.fold' which has the following type:
    //   ('State -> 'T -> 'State) -> 'State -> list<'T> -> 'State
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
