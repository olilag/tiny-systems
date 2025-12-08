// ----------------------------------------------------------------------------
// 02 - "Drag down" formula expanding
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

type Sheet = Map<Address, Expr>

// ----------------------------------------------------------------------------
// Drag down expansion
// ----------------------------------------------------------------------------

let rec relocateReferences (srcCol, srcRow) (tgtCol, tgtRow) (srcExpr: Expr) =
    // Replace references in expression 'srcExpr' in a way that
    // corresponds to moving the expression from address (srcRow, srcCol)
    // to address (tgtRow, tgtCol). So for example, if a formula 'A1+A2' is
    // moved from 'A3' to 'B10' then it should change to 'B8+B9' (address
    // is incremented by column difference 1 and row difference 7)
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


let expand (srcCol, srcRow) (tgtCol, tgtRow) (sheet: Sheet) : Sheet =
    // Expand formula at address (srcCol, srcRow) to all the cells
    // between itself and target cell at address (tgtCol, tgtRow) and
    // add the new formulas to the given sheet, returning the new sheet.
    //
    // HINT: You can use list comprehension with 'for .. in .. do' and
    // 'yield' or you can use 'List.init'. The comprehension is nicer,
    // but you need to figure out the right syntax! Once you generate
    // new cells, you can add them to the Map using List.fold (with the
    // sheet as the current state, updated in each step using Map.add).
    let colStep = if srcCol <= tgtCol then 1 else -1
    let rowStep = if srcRow <= tgtRow then 1 else -1

    let expr =
        match sheet.TryFind(srcCol, srcRow) with
        | Some e -> e
        | None -> failwith "Cell not found"

    let data =
        [ for col, row in Seq.allPairs (seq { srcCol..colStep..tgtCol }) (seq { srcRow..rowStep..tgtRow }) do
              yield (col, row), relocateReferences (srcCol, srcRow) (col, row) expr ]

    data |> List.fold (fun sheet (addr, expr) -> Map.add addr expr sheet) sheet


// ----------------------------------------------------------------------------
// Simple recursive evaluator
// ----------------------------------------------------------------------------

let rec eval (sheet: Sheet) expr =
    match expr with
    | Const v -> v
    | Reference addr ->
        match sheet.TryFind addr with
        | Some expr -> eval sheet expr
        | None -> Error "Missing value"
    | Function(name, args) -> evalFunction sheet name args

and evalFunction (sheet: Sheet) name args =
    match name with
    | "+" -> evalBinOp sheet args (fun a b -> a + b)
    | "*" -> evalBinOp sheet args (fun a b -> a * b)
    | _ -> Error(sprintf "Unknown function %s" name)

and evalBinOp (sheet: Sheet) args f =
    match args with
    | [ a; b ] ->
        match eval sheet a, eval sheet b with
        | Number a, Number b -> Number(f a b)
        | _ -> Error "Invalid argument types"
    | _ -> Error "Invalid number of arguments"


// ----------------------------------------------------------------------------
// Helpers and test cases
// ----------------------------------------------------------------------------

let addr (s: string) =
    let col = int s.[0] - int 'A' + 1
    let row = int s.[1..]
    Address(col, row)


let fib =
    [ addr "A1", Const(Number 0)
      addr "A2", Const(Number 1)
      addr "A3", Function("+", [ Reference(addr "A1"); Reference(addr "A2") ]) ]
    |> Map.ofList
    |> expand (addr "A3") (addr "A10")

// Should return: Number 13
eval fib (Reference(addr "A8"))

// Should return: Number 21
eval fib (Reference(addr "A9"))

// Should return: Number 34
eval fib (Reference(addr "A10"))

// Should return: Error "Missing value"
eval fib (Reference(addr "A11"))


// Column 'A' is a sequence of numbers increasing by 1
// Column 'B' is the factorial of the corresponding number
// i.e.: Bn = An * B(n-1) = An * A(n-1)!
let fac =
    [ addr "A2", Const(Number 1)
      addr "A3", Function("+", [ Reference(addr "A2"); Const(Number 1) ])
      addr "B1", Const(Number 1)
      addr "B2", Function("*", [ Reference(addr "A2"); Reference(addr "B1") ]) ]
    |> Map.ofList
    |> expand (addr "A3") (addr "A11")
    |> expand (addr "B2") (addr "B11")

// A6 should be 5, B6 should be 120
eval fac (Reference(addr "A6"))
eval fac (Reference(addr "B6"))

// A11 should be 10, B11 should be 3628800
eval fac (Reference(addr "A11"))
eval fac (Reference(addr "B11"))
