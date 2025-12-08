// ----------------------------------------------------------------------------
// 06 - Absolute and relative addresses
// ----------------------------------------------------------------------------

// NOTE: Location can be either fixed (absolute) or normal (relative)
// Address is used in 'Reference' and can be either. Raw address is
// actual location in a sheet and this remains just a pair of numbers.
type Location =
    | Fixed of int
    | Normal of int

type RawAddress = int * int
type Address = Location * Location

type Value =
    | Number of int
    | String of string
    | Error of string

type Expr =
    | Const of Value
    | Reference of Address
    | Function of string * Expr list

let (|RawReference|_|) addr =
    match addr with
    | Reference(Fixed col, Fixed row)
    | Reference(Normal col, Fixed row)
    | Reference(Fixed col, Normal row)
    | Reference(Normal col, Normal row) -> Some(col, row)
    | _ -> None


type CellNode =
    { mutable Value: Value
      mutable Expr: Expr
      Updated: Event<unit> }

// NOTE: Sheet is now indexed by raw address
type LiveSheet = Map<RawAddress, CellNode>

// ----------------------------------------------------------------------------
// Reactive evaluation and graph construction
// ----------------------------------------------------------------------------

let rec eval (sheet: LiveSheet) expr =
    match expr with
    | Const v -> v
    | RawReference addr ->
        match sheet.TryFind addr with
        | Some node -> node.Value
        | None -> Error "Missing value"
    | Function(name, args) -> evalFunction sheet name args
    | _ -> failwith "should be unreachable"

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

let rec collectReferences expr : list<RawAddress> =
    match expr with
    | Const _ -> []
    | RawReference addr -> [ addr ]
    | Function(_, args) -> args |> List.collect collectReferences
    | _ -> failwith "should be unreachable"

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

let relocateLocation (loc: Location) (by: int) : Location =
    // Implement this helper which relocates only relative locations.
    // It makes updating relocateReferences easier!
    match loc with
    | Normal loc -> Normal(loc + by)
    | fix -> fix


let rec relocateReferences (srcCol, srcRow) (tgtCol, tgtRow) (srcExpr: Expr) =
    // This needs to be updated to only relocate relative references!
    let colDiff = tgtCol - srcCol
    let rowDiff = tgtRow - srcRow

    match srcExpr with
    | Reference(col, row) -> Reference(relocateLocation col colDiff, relocateLocation row rowDiff)
    | Function(name, args) ->
        Function(
            name,
            args
            |> List.map (fun arg -> relocateReferences (srcCol, srcRow) (tgtCol, tgtRow) arg)
        )
    | e -> e


let expand (srcCol, srcRow) (tgtCol, tgtRow) (sheet: LiveSheet) : LiveSheet =
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
    match v with
    | Number x -> string x
    | String s -> s
    | Error msg -> $"<span class='e'>{msg}</span>"

let display (sheet: LiveSheet) =
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
// Helpers and continents demo
// ----------------------------------------------------------------------------

let raddr (s: string) : RawAddress =
    let col = int s.[0] - int 'A' + 1
    let row = int s.[1..]
    RawAddress(col, row)


let addr (s: string) : Address =
    // This is tricky to get right. See the test cases below.
    // You can use regex magic, or have a bunch of nested ifs -
    // starting with a check if s.[0] = '$' etc. You could also convert
    // string to list using List.ofSeq and use pattern matching.
    match List.ofSeq s with
    | '$' :: col :: '$' :: row ->
        let col, row = raddr $"{col}{row |> List.toArray |> System.String}"
        Fixed col, Fixed row
    | '$' :: col :: row ->
        let col, row = raddr $"{col}{row |> List.toArray |> System.String}"
        Fixed col, Normal row
    | col :: '$' :: row ->
        let col, row = raddr $"{col}{row |> List.toArray |> System.String}"
        Normal col, Fixed row
    | col :: row ->
        let col, row = raddr $"{col}{row |> List.toArray |> System.String}"
        Normal col, Normal row
    | _ -> failwith "should be unreachable"


addr "C10" = (Normal 3, Normal 10)
addr "$C10" = (Fixed 3, Normal 10)
addr "C$10" = (Normal 3, Fixed 10)
addr "$C$10" = (Fixed 3, Fixed 10)


let continents =
    [ "Asia", 4753079, 31033
      "Africa", 1460481, 29648
      "Europe", 740433, 22134
      "North America", 604182, 21330
      "South America", 439719, 17461
      "Australia/Oceania", 46004, 8486
      "Antarctica", 0, 13720 ]

let wsheet0 =
    [ // Column headers
      yield raddr "A1", Const(String "Continent")
      yield raddr "B1", Const(String "Population (thousands)")
      yield raddr "C1", Const(String "Area (thousands km^2)")

      // Fill rows of the data table
      for i, (cont, pop, area) in Seq.indexed continents do
          yield raddr $"A{i + 2}", Const(String cont)
          yield raddr $"B{i + 2}", Const(Number pop)
          yield raddr $"C{i + 2}", Const(Number area)

      // Add summary row for the world
      yield raddr "A9", Const(String "World")
      yield raddr "B9", Const(Number 8043898)
      yield raddr "C9", Const(Number 143812)

      // Add relative population
      yield raddr "D1", Const(String "Population (%)")
      yield
          raddr "D2",
          Function(
              "/",
              [ Function("*", [ Reference(addr "B2"); Const(Number 100) ])
                Reference(addr "$B$9") ]
          )

      // Add relative area
      yield raddr "E1", Const(String "Area (%)")
      yield
          raddr "E2",
          Function(
              "/",
              [ Function("*", [ Reference(addr "C2"); Const(Number 100) ])
                Reference(addr "$C$9") ]
          )

      // Add density of the region
      yield raddr "F1", Const(String "Density (pop/km^2)")
      yield raddr "F2", Function("/", [ Reference(addr "B2"); Reference(addr "C2") ]) ]
    |> makeSheet

// Display the initial sheet
display wsheet0

// Now expand all the calculations
let wsheet =
    wsheet0
    |> expand (raddr "D2") (raddr "D9")
    |> expand (raddr "E2") (raddr "E9")
    |> expand (raddr "F2") (raddr "F9")

// ...and display the resulting sheet!
display wsheet
