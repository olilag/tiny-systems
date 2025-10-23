// ----------------------------------------------------------------------------
// 06 - Add support for more elegant programs with GOSUB
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
    | Print of Expression list
    | Input of string
    | Stop
    // NOTE: Add the GOSUB jump and RETURN commands
    | GoSub of int
    | Return

type State =
    { Program: list<int * Command>
      Variables: Map<string, Value>
      Random: System.Random
      Stack: list<int> }

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
    | GoSub(jump) ->
        let newState =
            { state with
                Stack = line :: state.Stack }

        let goto = getLine newState jump
        runCommand newState goto
    | Return ->
        match state.Stack with
        | ret :: rest ->
            let newState = { state with Stack = rest }
            runNextLine newState ret
        | [] -> failwith "return stack empty"

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
      Random = System.Random()
      Stack = [] }

let nim =
    [ Some 10, Assign("M", num 20)
      Some 20, Assign("U", num 1)
      Some 30, GoSub(100)
      Some 40, Assign("U", num 2)
      Some 50, GoSub(100)
      Some 60, Goto(20)
      Some 100, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
      Some 110,
      Print
          [ str "PLAYER "
            var "U"
            str ": YOU CAN TAKE BETWEEN 1 AND "
            Function("MIN", [ num 5; var "M" ])
            str " MATCHES\n" ]
      Some 120, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
      Some 130, Input("P")
      Some 140, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 120)
      Some 150, Assign("M", var "M" .- var "P")
      Some 160, If(var "M" .= num 0, Goto 200)
      Some 170, Return
      Some 200, Print [ str "PLAYER "; var "U"; str " WINS!" ]
      None, Run ]

runInputs empty nim |> ignore
