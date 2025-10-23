// ----------------------------------------------------------------------------
// 05 - A few more functions and operators
// ----------------------------------------------------------------------------
module TinyBASIC

type Value =
    | StringValue of string
    | NumberValue of int
    | BoolValue of bool

type Expression =
    | Const of Value
    | Function of string * Expression list
    | Variable of string

type Command =
    | Run
    | Goto of int
    | Assign of string * Expression
    | If of Expression * Command
    | Clear
    | Poke of Expression * Expression * Expression
    // NOTE: Input("X") reads a number from console and assigns it to X;
    // Stop terminates the program; I also modified Print to take a list of
    // expressions instead of just one (which is what C64 supports too).
    | Print of Expression list
    | Input of string
    | Stop

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
    | "MIN" -> binaryNumOp min argVals
    | n -> failwith ("unknown function: " + n)

let rec runCommand state (line, cmd) =
    match cmd with
    | Print(expr) ->
        List.map (fun e -> printValue (evalExpression state e)) expr |> ignore
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
    | Input(varName) ->
        let mutable success, parsed = System.Int32.TryParse(System.Console.ReadLine())

        while not success do
            let s, p = System.Int32.TryParse(System.Console.ReadLine())
            success <- s
            parsed <- p

        let newState =
            { state with
                Variables = Map.add varName (NumberValue parsed) state.Variables }

        runNextLine newState line
    | Stop -> state

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
      Random = System.Random() }

// NOTE: A simple game you should be able to run now! :-)
let nim =
    [ Some 10, Assign("M", num 20)
      Some 20, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
      Some 30,
      Print
          [ str "PLAYER 1: YOU CAN TAKE BETWEEN 1 AND "
            "MIN" @ [ num 5; var "M" ]
            str " MATCHES\n" ]
      Some 40, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
      Some 50, Input("P")
      Some 60, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 40)
      Some 70, Assign("M", var "M" .- var "P")
      Some 80, If(var "M" .= num 0, Goto 200)
      Some 90, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
      Some 100,
      Print
          [ str "PLAYER 2: YOU CAN TAKE BETWEEN 1 AND "
            "MIN" @ [ num 5; var "M" ]
            str " MATCHES\n" ]
      Some 110, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
      Some 120, Input("P")
      Some 130, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 110)
      Some 140, Assign("M", var "M" .- var "P")
      Some 150, If(var "M" .= num 0, Goto 220)
      Some 160, Goto 20
      Some 200, Print [ str "PLAYER 1 WINS!" ]
      Some 210, Stop
      Some 220, Print [ str "PLAYER 2 WINS!" ]
      Some 230, Stop
      None, Run ]

runInputs empty nim |> ignore
