// ----------------------------------------------------------------------------
// 05 - Add a simple data type - tuples
// ----------------------------------------------------------------------------

type Value =
    | ValNum of int
    | ValClosure of string * Expression * VariableContext
    // NOTE: A tuple value consisting of two other values.
    // (Think about why we have 'Value' here but 'Expression'
    // in the case of 'ValClosure' above!)
    | ValTuple of Value * Value

and Expression =
    | Constant of int
    | Binary of string * Expression * Expression
    | Variable of string
    | Unary of string * Expression
    | If of Expression * Expression * Expression
    | Application of Expression * Expression
    | Lambda of string * Expression
    | Let of string * Expression * Expression
    // NOTE: 'Tuple' represents two-element tuple constructor
    // and 'TupleGet' the destructor (accessing a value)
    // Use 'true' for #1 element, 'false' for #2. This is not
    // particularly descriptive, but it works OK enough.
    | Tuple of Expression * Expression
    | TupleGet of bool * Expression

and VariableContext = Map<string, Value>

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evaluate (ctx: VariableContext) e =
    match e with
    | Constant n -> ValNum n
    | Binary(op, e1, e2) ->
        let v1 = evaluate ctx e1
        let v2 = evaluate ctx e2

        match v1, v2 with
        | ValNum n1, ValNum n2 ->
            match op with
            | "+" -> ValNum(n1 + n2)
            | "*" -> ValNum(n1 * n2)
            | _ -> failwith "unsupported binary operator"
        | _ -> failwith "invalid argument of binary operator"
    | Variable(v) ->
        match ctx.TryFind v with
        | Some res -> res
        | _ -> failwith ("unbound variable: " + v)
    | Unary(op, e) ->
        let v = evaluate ctx e

        match v with
        | ValNum v ->
            match op with
            | "-" -> ValNum(-v)
            | _ -> failwith "unsupported binary operator"
        | _ -> failwith "unsupported value type in unary operation"
    | If(cond, tbranch, fbranch) ->
        let cval = evaluate ctx cond

        match cval with
        | ValNum cval ->
            if cval = 1 then
                evaluate ctx tbranch
            else
                evaluate ctx fbranch
        | _ -> failwith "unsupported value type in condition"
    | Lambda(v, e) -> ValClosure(v, e, ctx)
    | Application(e1, e2) ->
        let closure = evaluate ctx e1

        match closure with
        | ValClosure(name, body, cctx) ->
            let argval = evaluate ctx e2
            let cctx = Map.add name argval cctx

            evaluate cctx body
        | _ -> failwith "first argument is not a function"
    | Let(v, e1, e2) ->
        let lval = evaluate ctx e1
        let lctx = Map.add v lval ctx

        evaluate lctx e2
    | Tuple(e1, e2) ->
        let v1 = evaluate ctx e1
        let v2 = evaluate ctx e2

        ValTuple(v1, v2)
    | TupleGet(b, e) ->
        let t = evaluate ctx e

        match t with
        | ValTuple(v1, v2) -> if b then v1 else v2
        | _ -> failwith "expression is not a tuple"


// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Data types - simple tuple example (using the e#1, e#2 notation for field access)
//   (2*21, 123)#1
//   (2*21, 123)#2
let ed1 =
    TupleGet(true, Tuple(Binary("*", Constant(2), Constant(21)), Constant(123)))

evaluate Map.empty ed1

let ed2 =
    TupleGet(false, Tuple(Binary("*", Constant(2), Constant(21)), Constant(123)))

evaluate Map.empty ed2

// Data types - trying to get a first element of a value
// that is not a tuple (This makes no sense and should fail)
//   (42)#1
let ed3 = TupleGet(true, Constant(42))
evaluate Map.empty ed3
