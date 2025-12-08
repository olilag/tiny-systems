// ----------------------------------------------------------------------------
// 04 - Reactive event-based computation
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
      // NOTE: Added event that will be triggered when the
      // expression and value of the node is changed.
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
    // Collect the addresses of all references that appear in the
    // expression 'expr'. This needs to call itself recursively for all
    // arguments of 'Function' and concatenate the returned lists.
    // HINT: This looks nice if you use 'List.collect'.
    match expr with
    | Const _ -> []
    | Reference addr -> [ addr ]
    | Function(_, args) -> args |> List.collect collectReferences


let makeNode (sheet: LiveSheet) expr =
    //   Add handling of 'Update' events!
    //
    // * When creating a node, we need to create a new event and
    //   set it as the 'Updated' event of the returned node.
    // * We then need to define 'update' function that will be triggered
    //   when any of the cells on which this one depends change. In the
    //   function, re-evaluate the formula, set the new value and trigger
    //   our Updated event to notify other cells.
    // * Before returning, use 'collectReferences' to find all cells on which
    //   this one depends and add 'update' as the handler of their
    //   'Updated' event
    //
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
    // For now, we ignore the fact that the new expression may have
    // different set of references than the one we are replacing.
    // So, we can just get the node, set the new expression and value
    // and trigger the Updated event!
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
// Helpers and test cases
// ----------------------------------------------------------------------------

let addr (s: string) =
    let col = int s.[0] - int 'A' + 1
    let row = int s.[1..]
    Address(col, row)

// Simple spreadsheet that performs conversion between Celsius and Fahrenheit
// To convert F to C, we put value in F into B1 and read the result in C1
// To convert C to F, we put value in C into B2 and read the result in C2
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
      // Add formula for Celsius to Fahrenheit conversion to 'C2'
      addr "C2",
      Function(
          "+",
          [ Function("/", [ Function("*", [ Const(Number 9); Reference(addr "B2") ]); Const(Number 5) ])
            Const(Number 32) ]
      ) ]
    |> makeSheet

// Fahrenheit to Celsius conversions

// Should return: -17
updateNode (addr "B1") tempConv (Const(Number 0))
eval tempConv (Reference(addr "C1"))
// Should return: 0
updateNode (addr "B1") tempConv (Const(Number 32))
eval tempConv (Reference(addr "C1"))
// Should return: 37
updateNode (addr "B1") tempConv (Const(Number 100))
eval tempConv (Reference(addr "C1"))

// Celsius to Fahrenheit conversions

// Should return: 32
updateNode (addr "B2") tempConv (Const(Number 0))
eval tempConv (Reference(addr "C2"))
// Should return: 212
updateNode (addr "B2") tempConv (Const(Number 100))
eval tempConv (Reference(addr "C2"))
// Should return: 100
updateNode (addr "B2") tempConv (Const(Number 38))
eval tempConv (Reference(addr "C2"))
