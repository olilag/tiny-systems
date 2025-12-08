// ----------------------------------------------------------------------------
// 05 - Rendering sheets as HTML
// ----------------------------------------------------------------------------

type Address = int * int

type Value =
    | Number of int
    | String of string
    | Error of string

type Expr =
    | Const of Value
    | Reference of Address
    | Function of string * Expr list

type CellNode =
    { mutable Value: Value
      mutable Expr: Expr
      Updated: Event<unit> }

type LiveSheet = Map<Address, CellNode>

// ----------------------------------------------------------------------------
// Reactive evaluation and graph construction
// ----------------------------------------------------------------------------

let rec eval (sheet: LiveSheet) expr =
    match expr with
    | Const v -> v
    | Reference addr ->
        match sheet.TryFind addr with
        | Some node -> node.Value
        | None -> Error "Missing value"
    | Function(name, args) -> evalFunction sheet name args

and evalFunction (sheet: LiveSheet) name args =
    match name with
    | "+" -> evalBinOp sheet args (fun a b -> a + b)
    | "*" -> evalBinOp sheet args (fun a b -> a * b)
    | "-" -> evalBinOp sheet args (fun a b -> a - b)
    | "/" -> evalBinOp sheet args (fun a b -> a / b)
    | _ -> Error(sprintf "Unknown function %s" name)

and evalBinOp (sheet: LiveSheet) args f =
    match args with
    | [ a; b ] ->
        match eval sheet a, eval sheet b with
        | Number a, Number b -> Number(f a b)
        | a, b -> Error(sprintf "Invalid argument types (%A, %A)" a b)
    | _ -> Error "Invalid number of arguments"

let rec collectReferences (expr: Expr) : Address list =
    match expr with
    | Const _ -> []
    | Reference addr -> [ addr ]
    | Function(_, args) -> args |> List.collect collectReferences

let makeNode (sheet: LiveSheet) expr =
    let value = eval sheet expr

    let node =
        { Expr = expr
          Value = value
          Updated = Event<unit>() }

    let updateHandler = Handler(fun _ () -> node.Value <- eval sheet node.Expr)
    let deps = collectReferences expr

    for addr in deps do
        sheet[addr].Updated.Publish.AddHandler updateHandler

    node

let updateNode addr (sheet: LiveSheet) expr =
    let value = eval sheet expr
    sheet[addr].Expr <- expr
    sheet[addr].Value <- value
    sheet[addr].Updated.Trigger()

let makeSheet list =
    list
    |> List.fold (fun sheet (addr, expr) -> Map.add addr (makeNode sheet expr) sheet) Map.empty

// ----------------------------------------------------------------------------
// Drag down expansion
// ----------------------------------------------------------------------------

let rec relocateReferences (srcCol, srcRow) (tgtCol, tgtRow) (srcExpr: Expr) =
    let colDiff = tgtCol - srcCol
    let rowDiff = tgtRow - srcRow

    match srcExpr with
    | Reference(col, row) -> Reference(col + colDiff, row + rowDiff)
    | Function(name, args) ->
        Function(
            name,
            args
            |> List.map (fun arg -> relocateReferences (srcCol, srcRow) (tgtCol, tgtRow) arg)
        )
    | e -> e

let expand (srcCol, srcRow) (tgtCol, tgtRow) (sheet: LiveSheet) : LiveSheet =
    // This needs to call 'makeNode' and add the resulting node,
    // instead of just adding the expression to the map as is.
    let colStep = if srcCol <= tgtCol then 1 else -1
    let rowStep = if srcRow <= tgtRow then 1 else -1

    let expr =
        match sheet.TryFind(srcCol, srcRow) with
        | Some e -> e.Expr
        | None -> failwith "Cell not found"

    let data =
        [ for col, row in
              Seq.allPairs (seq { srcCol..colStep..tgtCol }) (seq { srcRow..rowStep..tgtRow })
              |> Seq.skip 1 do
              yield (col, row), relocateReferences (srcCol, srcRow) (col, row) expr ]

    data
    |> List.fold (fun sheet (addr, expr) -> Map.add addr (makeNode sheet expr) sheet) sheet

// ----------------------------------------------------------------------------
// Rendering sheets as HTML
// ----------------------------------------------------------------------------

open System.IO
open System.Diagnostics

let displayValue (v: Value) : string =
    // Turn the given value into a string representing HTML
    // You can use the following to create an error string in red.
    match v with
    | Number x -> string x
    | String s -> s
    | Error msg -> $"<span class='e'>{msg}</span>"

let display (sheet: LiveSheet) =
    // Find the greates row and column index
    let maxCol = sheet |> Map.keys |> Seq.map (fun (col, _) -> col) |> Seq.max
    let maxRow = sheet |> Map.keys |> Seq.map (fun (_, row) -> row) |> Seq.max

    let f = Path.GetTempFileName() + ".html"
    use wr = new StreamWriter(File.OpenWrite f)

    wr.Write(
        """<html><head>
      <style>
        * { font-family:sans-serif; margin:0px; padding:0px; border-spacing:0; } 
        th, td { border:1px solid black; border-collapse:collapse; padding:4px 10px 4px 10px }
        body { padding:50px } .e { color: red; } 
        th { background:#606060; color:white; } 
      </style>
    </head><body><table>"""
    )

    // Write column headings
    wr.Write "<tr><th></th>"

    for col in 1..maxCol do
        wr.Write $"<th>{char (int 'A' + col - 1)}</th>"

    wr.Write "</tr>"

    // Write row headings and data
    for row in 1..maxRow do
        wr.Write $"<tr><th>{row}</th>"

        for col in 1..maxCol do
            let s =
                match sheet.TryFind(col, row) with
                | Some node -> displayValue node.Value
                | None -> "-"

            wr.Write $"<td>{s}</td>"

        wr.Write "</tr>"

    wr.Write "</table></body></html>"
    wr.Close()
    Process.Start("firefox", f)


// ----------------------------------------------------------------------------
// Helpers and test cases
// ----------------------------------------------------------------------------

let addr (s: string) =
    let col = int s.[0] - int 'A' + 1
    let row = int s.[1..]
    Address(col, row)

// NOTE: Let's visualize the Fibbonacci spreadsheet from Step 2!
let fib =
    [ addr "A1", Const(Number 0)
      addr "A2", Const(Number 1)
      addr "A3", Function("+", [ Reference(addr "A1"); Reference(addr "A2") ]) ]
    |> makeSheet
    |> expand (addr "A3") (addr "A10")

display fib

// NOTE: Let's visualize the Factorial spreadsheet from Step 2!
let fac =
    [ addr "A2", Const(Number 1)
      addr "A3", Function("+", [ Reference(addr "A2"); Const(Number 1) ])
      addr "B1", Const(Number 1)
      addr "B2", Function("*", [ Reference(addr "A2"); Reference(addr "B1") ]) ]
    |> makeSheet
    |> expand (addr "A3") (addr "A11")
    |> expand (addr "B2") (addr "B11")

display fac

// NOTE: Let's visualize the Temp convertor spreadsheet from Step 4!
let tempConv =
    [ addr "A1", Const(String "F to C")
      addr "B1", Const(Number 0)
      addr "C1",
      Function(
          "/",
          [ Function("*", [ Function("-", [ Reference(addr "B1"); Const(Number 32) ]); Const(Number 5) ])
            Const(Number 9) ]
      )
      addr "A2", Const(String "C to F")
      addr "B2", Const(Number 0)
      addr "C2",
      Function(
          "+",
          [ Function("/", [ Function("*", [ Const(Number 9); Reference(addr "B2") ]); Const(Number 5) ])
            Const(Number 32) ]
      ) ]
    |> makeSheet

display tempConv
