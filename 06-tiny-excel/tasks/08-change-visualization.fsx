// ----------------------------------------------------------------------------
// 08 - Adding change visualization
// ----------------------------------------------------------------------------

type Location =
    | Fixed of int
    | Normal of int

type RawAddress = int * int
type Address = Location * Location

let (|RawAddress|) addr =
    match addr with
    | Fixed col, Fixed row
    | Normal col, Fixed row
    | Fixed col, Normal row
    | Normal col, Normal row -> col, row

type Value =
    | Number of int
    | String of string
    | Error of string
    | Array of Value list

type Expr =
    | Const of Value
    | Reference of Address
    | Function of string * Expr list
    | Range of Address * Address

type CellNode =
    { mutable Value: Value
      mutable Expr: Expr
      // NOTE: Set to 'true' when the cell is created or modified.
      // Used to highlight changes. Touched can later be cleared.
      mutable Touched: bool
      Updated: Event<unit> }

type LiveSheet = Map<RawAddress, CellNode>

module List =
    /// Attempts to map all elements of the input list and
    /// succeeds only if each of the operation succeeds.
    /// If any mapping operation fails, returns 'None'.
    let rec tryMap (f: 'a -> 'b option) (xs: 'a list) : 'b list option =
        match xs with
        | [] -> Some []
        | x :: xs ->
            match f x with
            | None -> None
            | Some x ->
                match tryMap f xs with
                | None -> None
                | Some xs -> Some(x :: xs)

let clean (sheet: LiveSheet) =
    // Implement a helper that sets Touched <- false
    // for all cell nodes in a given sheet.
    for _, node in Map.toSeq sheet do
        node.Touched <- false

// ----------------------------------------------------------------------------
// Reactive evaluation and graph construction
// ----------------------------------------------------------------------------

let rangeAddresses (sCol, sRow) (eCol, eRow) =
    let colStep = if sCol <= eCol then 1 else -1
    let rowStep = if sRow <= eRow then 1 else -1

    Seq.allPairs (seq { sCol..colStep..eCol }) (seq { sRow..rowStep..eRow })
    |> Seq.toList

let rec eval (sheet: LiveSheet) expr =
    //   This needs to be modified in two ways.
    //
    // * Handle the 'Range' case. You will need to extract the raw
    //   addresses of the two corners and iterate over all cells in the range.
    //   You can then use 'List.tryMapAll' (from the lecture) to get all the
    //   values. If they are all there, return 'Array', otherwise 'Error'.
    //
    // * Add 'SUM' function that takes one argument which is 'Array'
    //   (defined by range) and sums all elements of the array. This only
    //   works if they are all numerical. Use 'List.tryMapAll' here too!
    //
    match expr with
    | Const v -> v
    | Reference(RawAddress addr) ->
        match sheet.TryFind addr with
        | Some node -> node.Value
        | None -> Error "Missing value"
    | Function(name, args) -> evalFunction sheet name args
    | Range(RawAddress s, RawAddress e) ->
        match rangeAddresses s e |> List.tryMap (fun addr -> sheet.TryFind addr) with
        | Some l -> Array(l |> List.map (fun node -> eval sheet node.Expr))
        | None -> Error "Missing value"

and evalFunction (sheet: LiveSheet) name args =
    match name with
    | "+" -> evalBinOp sheet args (fun a b -> a + b)
    | "*" -> evalBinOp sheet args (fun a b -> a * b)
    | "-" -> evalBinOp sheet args (fun a b -> a - b)
    | "/" -> evalBinOp sheet args (fun a b -> a / b)
    | "SUM" -> evalSum sheet args
    | _ -> Error(sprintf "Unknown function %s" name)

and evalBinOp (sheet: LiveSheet) args f =
    match args with
    | [ a; b ] ->
        match eval sheet a, eval sheet b with
        | Number a, Number b -> Number(f a b)
        | a, b -> Error $"Invalid argument types ({a}, {b})"
    | _ -> Error "Invalid number of arguments"

and evalSum (sheet: LiveSheet) args =
    match args with
    | [ arr ] ->
        match eval sheet arr with
        | Array vals ->
            match
                vals
                |> List.tryMap (fun v ->
                    match v with
                    | Number x -> Some x
                    | _ -> None)
            with
            | Some nums -> Number(nums |> List.sum)
            | None -> Error "Invalid type in array"
        | v -> Error $"Invalid argument type ({v})"
    | _ -> Error "Invalid number of arguments"

let rec collectReferences expr : list<RawAddress> =
    match expr with
    | Const _ -> []
    | Reference(RawAddress addr) -> [ addr ]
    | Function(_, args) -> args |> List.collect collectReferences
    | Range(RawAddress s, RawAddress e) -> rangeAddresses s e

let makeNode (sheet: LiveSheet) expr =
    let value = eval sheet expr

    let node =
        { Expr = expr
          Value = value
          Updated = Event<unit>()
          Touched = true }

    let updateHandler =
        Handler(fun _ () ->
            node.Value <- eval sheet node.Expr
            node.Touched <- true)

    let deps = collectReferences expr

    for addr in deps do
        sheet[addr].Updated.Publish.AddHandler updateHandler

    node

let updateNode addr (sheet: LiveSheet) expr =
    let value = eval sheet expr
    sheet[addr].Expr <- expr
    sheet[addr].Value <- value
    sheet[addr].Updated.Trigger()
    sheet[addr].Touched <- true

let makeSheet list =
    list
    |> List.fold (fun sheet (addr, expr) -> Map.add addr (makeNode sheet expr) sheet) Map.empty

// ----------------------------------------------------------------------------
// Drag down expansion
// ----------------------------------------------------------------------------

let relocateLocation (loc: Location) (by: int) : Location =
    match loc with
    | Normal loc -> Normal(loc + by)
    | fix -> fix

let rec relocateReferences (srcCol, srcRow) (tgtCol, tgtRow) (srcExpr: Expr) =
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
    | Range((sCol, sRow), (eCol, eRow)) ->
        Range(
            (relocateLocation sCol colDiff, relocateLocation sRow rowDiff),
            (relocateLocation eCol colDiff, relocateLocation eRow rowDiff)
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
    | Array vals -> failwith ""

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
        body { padding:50px } .e { color: red; } .touched { background: green; }
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
            match sheet.TryFind(col, row) with
            | Some node ->
                let s = displayValue node.Value
                let style = if node.Touched then " class='touched'" else ""
                wr.Write $"<td{style}>{s}</td>"
            | None -> wr.Write "<td>-</td>"

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

let continents =
    [ "Asia", 4753079, 31033
      "Africa", 1460481, 29648
      "Europe", 740433, 22134
      "North America", 604182, 21330
      "South America", 439719, 17461
      "Australia/Oceania", 46004, 8486
      "Antarctica", 0, 13720 ]

let wsheet0 =
    [ yield raddr "A1", Const(String "Continent")
      yield raddr "B1", Const(String "Population (thousands)")
      yield raddr "C1", Const(String "Area (thousands km^2)")
      for i, (cont, pop, area) in Seq.indexed continents do
          yield raddr $"A{i + 2}", Const(String cont)
          yield raddr $"B{i + 2}", Const(Number pop)
          yield raddr $"C{i + 2}", Const(Number area)
      yield raddr "A9", Const(String "World")
      yield raddr "B9", Function("SUM", [ Range(addr "B2", addr "B8") ])
      yield raddr "C9", Function("SUM", [ Range(addr "C2", addr "C8") ])
      yield raddr "D1", Const(String "Population (%)")
      yield
          raddr "D2",
          Function(
              "/",
              [ Function("*", [ Reference(addr "B2"); Const(Number 100) ])
                Reference(addr "$B$9") ]
          )
      yield raddr "E1", Const(String "Area (%)")
      yield
          raddr "E2",
          Function(
              "/",
              [ Function("*", [ Reference(addr "C2"); Const(Number 100) ])
                Reference(addr "$C$9") ]
          )
      yield raddr "F1", Const(String "Density (pop/km^2)")
      yield raddr "F2", Function("/", [ Reference(addr "B2"); Reference(addr "C2") ]) ]
    |> makeSheet

// Display the initial sheet
display wsheet0
clean wsheet0

// Now expand all the calculations
let wsheet =
    wsheet0
    |> expand (raddr "D2") (raddr "D9")
    |> expand (raddr "E2") (raddr "E9")
    |> expand (raddr "F2") (raddr "F9")

// ...and display the resulting sheet!
display wsheet
clean wsheet

// And now put some aliens into Antarctica!
// (see what cells need to be recomputed)
updateNode (raddr "B8") wsheet (Const(Number(1000000)))
display wsheet
clean wsheet
