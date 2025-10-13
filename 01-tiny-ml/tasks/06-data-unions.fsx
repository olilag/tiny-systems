// ----------------------------------------------------------------------------
// 06 - Add more data types - unions
// ----------------------------------------------------------------------------

type Value =
    | ValNum of int
    | ValClosure of string * Expression * VariableContext
    | ValTuple of Value * Value
    // NOTE: Value representing a union case. Again, we use 'bool':
    // 'true' for 'Case1' and 'false' for 'Case2'
    | ValCase of bool * Value

and Expression =
    | Constant of int
    | Binary of string * Expression * Expression
    | Variable of string
    | Unary of string * Expression
    | If of Expression * Expression * Expression
    | Application of Expression * Expression
    | Lambda of string * Expression
    | Let of string * Expression * Expression
    | Tuple of Expression * Expression
    | TupleGet of bool * Expression
    // NOTE: 'Case' represents creating a union value and 'Match' pattern
    // matching. You can read 'Match(e, v, e1, e2)' as F# pattern matching
    // of the form: 'match e with v -> e1 | v -> e2'
    | Case of bool * Expression
    | Match of Expression * string * Expression * Expression

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
    | Match(e, v, e1, e2) ->
        let c = evaluate ctx e

        match c with
        | ValCase(b, cv) ->
            let e = if b then e1 else e2
            let cctx = Map.add v cv ctx

            evaluate cctx e
        | _ -> failwith "expression is not a union"
    | Case(b, e) ->
        let v = evaluate ctx e

        ValCase(b, v)

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Data types - creating a union value
let ec1 = Case(true, Binary("*", Constant(21), Constant(2)))
evaluate Map.empty ec1

// Data types - working with union cases
//   match Case1(21) with Case1(x) -> x*2 | Case2(x) -> x*100
//   match Case2(21) with Case1(x) -> x*2 | Case2(x) -> x*100
let ec2 =
    Match(
        Case(true, Constant(21)),
        "x",
        Binary("*", Variable("x"), Constant(2)),
        Binary("*", Variable("x"), Constant(100))
    )

evaluate Map.empty ec2

let ec3 =
    Match(
        Case(false, Constant(21)),
        "x",
        Binary("*", Variable("x"), Constant(2)),
        Binary("*", Variable("x"), Constant(100))
    )

evaluate Map.empty ec3
