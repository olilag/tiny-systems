// ----------------------------------------------------------------------------
// 04 - Random function and (not quite correct) POKE
// ----------------------------------------------------------------------------
module TinyBASIC

type Value =
    | StringValue of string
    | NumberValue of int
    | BoolValue of bool

and Expression =
    | Const of Value
    | Function of string * Expression list
    | Variable of string

type Command =
    | Print of Expression
    | Run
    | Goto of int
    | Assign of string * Expression
    | If of Expression * Command
    // NOTE: Clear clears the screen and Poke(x, y, e) puts a string 'e' at
    // the console location (x, y). In C64, the actual POKE writes to a given
    // memory location, but we only use it for screen access here.
    | Clear
    | Poke of Expression * Expression * Expression

type State =
    { Program: list<int * Command>
      Variables: Map<string, Value>
      Random: System.Random }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value =
    match value with
    | StringValue s -> printf "%s" s
    | NumberValue n -> printf "%d" n
    | BoolValue b -> printf "%b" b

let getLine state line =
    let findFn = fun (l, _) -> l = line
    let res = List.tryFind findFn state.Program

    match res with
    | Some v -> v
    | None -> failwithf "no line with number: %d" line

let rec replaceOrAddLine (nLine, nCmd) prog =
    match prog with
    | [] -> [ nLine, nCmd ]
    | (cLine, _) :: prog when cLine = nLine -> (nLine, nCmd) :: prog
    | (cLine, _) :: _ when cLine > nLine -> (nLine, nCmd) :: prog
    | c :: prog -> c :: replaceOrAddLine (nLine, nCmd) prog

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

// NOTE: Helper function that makes it easier to implement '>' and '<' operators
// (takes a function 'int -> int -> bool' and "lifts" it into 'Value -> Value -> Value')
let binaryRelOp f args =
    match args with
    | [ NumberValue a; NumberValue b ] -> BoolValue(f a b)
    | _ -> failwith "expected two numerical arguments"

let binaryNumOp f args =
    match args with
    | [ NumberValue a; NumberValue b ] -> NumberValue(f a b)
    | _ -> failwith "expected two numerical arguments"

let binaryBoolOp f args =
    match args with
    | [ BoolValue a; BoolValue b ] -> BoolValue(f a b)
    | _ -> failwith "expected two boolean arguments"

let rec evalExpression state expr =
    // TODO: Add support for 'RND(N)' which returns a random number in range 0..N-1
    // and for binary operators ||, <, > (and the ones you have already, i.e., - and =).
    // To add < and >, you can use the 'binaryRelOp' helper above. You can similarly
    // add helpers for numerical operators and binary Boolean operators to make
    // your code a bit nicer.
    match expr with
    | Const v -> v
    | Function(name, args) -> evalFunction state name args
    | Variable name ->
        match state.Variables.TryFind name with
        | Some res -> res
        | _ -> failwith ("unbound variable: " + name)

and evalFunction state name args =
    let argVals = List.map (fun e -> evalExpression state e) args

    match name with
    | "=" ->
        match argVals with
        | [ v1; v2 ] -> BoolValue(v1 = v2)
        | _ -> failwith "incompatible arguments for '='"
    | "-" -> binaryNumOp (fun a b -> a - b) argVals
    | "||" -> binaryBoolOp (fun a b -> a || b) argVals
    | "<" -> binaryRelOp (fun a b -> a < b) argVals
    | ">" -> binaryRelOp (fun a b -> a > b) argVals
    | "RND" ->
        match argVals with
        | [ NumberValue m ] -> NumberValue(state.Random.Next m)
        | _ -> failwith "incompatible arguments"
    | n -> failwith ("unknown function: " + n)

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
    | Assign(name, expr) ->
        let v = evalExpression state expr

        let newState =
            { state with
                Variables = Map.add name v state.Variables }

        runNextLine newState line
    | If(condExpr, cmd) ->
        let cond = evalExpression state condExpr

        match cond with
        | BoolValue(true) -> runCommand state (line, cmd)
        | BoolValue(false) -> runNextLine state line
        | _ -> failwith "incompatible type for if condition"

    // TODO: Implement two commands for screen manipulation
    | Clear ->
        System.Console.Clear()
        runNextLine state line
    | Poke(x, y, chr) ->
        let x = evalExpression state x
        let y = evalExpression state y
        let chr = evalExpression state chr

        match x, y, chr with
        | NumberValue(x), NumberValue(y), StringValue(chr) ->
            System.Console.CursorLeft <- x
            System.Console.CursorTop <- y
            System.Console.Write chr
            runNextLine state line
        | _ -> failwith "invalid argument types for poke"

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
        let l = replaceOrAddLine (n, cmd) state.Program
        { state with Program = l }
    | None -> runCommand state (-1, cmd)

let runInputs state cmds =
    let foldFn = fun acc elem -> runInput acc elem
    List.fold foldFn state cmds

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// NOTE: Writing all the BASIC expressions is quite tedious, so this is a
// very basic (and terribly elegant) trick to make our task a bit easier.
// We define a couple of shortcuts and custom operators to construct expressions.
// With these, we can write e.g.:
//  'Function("RND", [Const(NumberValue 100)])' as '"RND" @ [num 100]' or
//  'Function("-", [Variable("I"); Const(NumberValue 1)])' as 'var "I" .- num 1'
let num v = Const(NumberValue v)
let str v = Const(StringValue v)
let var n = Variable n
let (.||) a b = Function("||", [ a; b ])
let (.<) a b = Function("<", [ a; b ])
let (.>) a b = Function(">", [ a; b ])
let (.-) a b = Function("-", [ a; b ])
let (.=) a b = Function("=", [ a; b ])
let (@) s args = Function(s, args)

let empty =
    { Program = []
      Variables = Map.empty
      Random = System.Random() } // TODO: Add random number generator!

// NOTE: Random stars generation. This has hard-coded max width and height (60x20)
// but you could use 'System.Console.WindowWidth'/'Height' here to make it nicer.
let stars =
    [ Some 10, Clear
      Some 20, Poke("RND" @ [ num System.Console.WindowWidth ], "RND" @ [ num System.Console.WindowHeight ], str "*")
      Some 30, Assign("I", num 100)
      Some 40, Poke("RND" @ [ num System.Console.WindowWidth ], "RND" @ [ num System.Console.WindowHeight ], str " ")
      Some 50, Assign("I", var "I" .- num 1)
      Some 60, If(var "I" .> num 1, Goto(40))
      Some 100, Goto(20)
      None, Run ]

// NOTE: Make the cursor invisible to get a nicer stars animation
System.Console.CursorVisible <- false
runInputs empty stars |> ignore
