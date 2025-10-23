// ----------------------------------------------------------------------------
// 03 - Add variables, conditionals and integer values
// ----------------------------------------------------------------------------
module TinyBASIC

type Value =
    | StringValue of string
    // NOTE: Added numerical and Boolean values
    | NumberValue of int
    | BoolValue of bool

and Expression =
    | Const of Value
    // NOTE: Added functions and variables. Functions  are used for both
    // functions (later) and binary operators (in this step). We use only
    // 'Function("-", [e1; e2])' and 'Function("=", [e1; e2])' in the demo.
    | Function of string * Expression list
    | Variable of string

and Command =
    | Print of Expression
    | Run
    | Goto of int
    // NOTE: Assign expression to a given variable and conditional that
    // runs a given Command only if the expression evaluates to 'BoolValue(true)'
    | Assign of string * Expression
    | If of Expression * Command

and State =
    { Program: list<int * Command>
      Context: VariableContext }

and VariableContext = Map<string, Value>

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value =
    match value with
    | StringValue s -> printfn "%s" s
    | NumberValue n -> printfn "%d" n
    | BoolValue b -> printfn "%b" b

let getLine state line =
    let findFn = fun (l, _) -> l = line
    let res = List.tryFind findFn state.Program

    match res with
    | Some v -> v
    | None -> failwithf "no line with number: %d" line

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evalExpression state expr =
    // TODO: Add support for 'Function' and 'Variable'. For now, handle just the two
    // functions we need, i.e. "-" (takes two numbers & returns a number) and "="
    // (takes two values and returns Boolean). Note that you can test if two
    // F# values are the same using '='. It works on values of type 'Value' too.
    //
    // HINT: You will need to pass the program state to 'evalExpression'
    // in order to be able to handle variables!
    match expr with
    | Const v -> v
    | Function(name, args) ->
        match name with
        | "=" ->
            match args with
            | [ e1; e2 ] ->
                let v1 = evalExpression state e1
                let v2 = evalExpression state e2

                BoolValue(v1 = v2)
            | _ -> failwith "incompatible arguments for '='"
        | "-" ->
            match args with
            | [ e1; e2 ] ->
                let v1 = evalExpression state e1
                let v2 = evalExpression state e2

                match (v1, v2) with
                | NumberValue(v1), NumberValue(v2) -> NumberValue(v1 - v2)
                | _ -> failwith "incompatible arguments for '-'"
            | _ -> failwith "incompatible arguments for '-'"
        | n -> failwith ("unknown function: " + n)
    | Variable name ->
        match state.Context.TryFind name with
        | Some res -> res
        | _ -> failwith ("unbound variable: " + name)

let rec runCommand state (line, cmd) =
    match cmd with
    | Print(expr) ->
        let v = evalExpression state expr
        printValue v
        runNextLine state line
    | Run ->
        let first = List.head state.Program
        runCommand state first
    | Goto(line) ->
        let goto = getLine state line
        runCommand state goto

    // TODO: Implement assignment and conditional. Assignment should run the
    // next line after setting the variable value. 'If' is a bit trickier:
    // * 'L1: IF TRUE THEN GOTO <L2>' will continue evaluating on line 'L2'
    // * 'L1: IF FALSE THEN GOTO <L2>' will continue on line after 'L1'
    // * 'L1: IF TRUE THEN PRINT "HI"' will print HI and continue on line after 'L1'
    //
    // HINT: If <e> evaluates to TRUE, you can call 'runCommand' recursively with
    // the command in the 'THEN' branch and the current line as the line number.
    | Assign(name, expr) ->
        let v = evalExpression state expr

        let newState =
            { state with
                Context = Map.add name v state.Context }

        runNextLine newState line
    | If(condExpr, cmd) ->
        let cond = evalExpression state condExpr

        match cond with
        | BoolValue(true) -> runCommand state (line, cmd)
        | BoolValue(false) -> runNextLine state line
        | _ -> failwith "incompatible type for if condition"

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

let empty = { Program = []; Context = Map.empty } // TODO: Add empty variables to the initial state!

let helloOnce =
    [ Some 10, Print(Const(StringValue "HELLO WORLD\n"))
      Some 10, Print(Const(StringValue "HELLO NPRG077\n"))
      None, Run ]

let helloInf =
    [ Some 20, Goto 10
      Some 10, Print(Const(StringValue "HELLO WORLD\n"))
      Some 10, Print(Const(StringValue "HELLO NPRG077\n"))
      None, Run ]

let testVariables =
    [ Some 10, Assign("S", Const(StringValue "HELLO WORLD\n"))
      Some 20, Assign("I", Const(NumberValue 1))
      Some 30, Assign("B", Function("=", [ Variable("I"); Const(NumberValue 1) ]))
      Some 40, Print(Variable "S")
      Some 50, Print(Variable "I")
      Some 60, Print(Variable "B")
      None, Run ]

// NOTE: Simpler test program without 'If" (just variables and '=' function)
runInputs empty testVariables |> ignore

let helloTen =
    [ Some 10, Assign("I", Const(NumberValue 10))
      Some 20, If(Function("=", [ Variable("I"); Const(NumberValue 1) ]), Goto(60))
      Some 30, Print(Const(StringValue "HELLO WORLD\n"))
      Some 40, Assign("I", Function("-", [ Variable("I"); Const(NumberValue 1) ]))
      Some 50, Goto 20
      Some 60, Print(Const(StringValue ""))
      None, Run ]

// NOTE: Prints hello world ten times using conditionals
runInputs empty helloTen |> ignore
