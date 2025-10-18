// ----------------------------------------------------------------------------
// 08 - Add unit and create a list value
// ----------------------------------------------------------------------------

type Value =
    | ValNum of int
    | ValClosure of string * Expression * VariableContext
    | ValTuple of Value * Value
    | ValCase of bool * Value
    // NOTE: A value that represents "empty value" and is
    // useful as the value for representing the empty list.
    | ValUnit

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
    | Case of bool * Expression
    | Match of Expression * string * Expression * Expression
    | Recursive of string * Expression * Expression
    // NOTE: An expression that evaluates to a unit value.
    // This exists in F# too and it is written as '()'
    | Unit

and VariableContext = Map<string, Lazy<Value>>

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
        | Some res -> res.Value
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
            let argval = lazy evaluate ctx e2
            let cctx = Map.add name argval cctx

            evaluate cctx body
        | _ -> failwith "first argument is not a function"
    | Let(v, e1, e2) ->
        let lval = lazy evaluate ctx e1
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
            let lv = lazy cv
            let cctx = Map.add v lv ctx

            evaluate cctx e
        | _ -> failwith "expression is not a union"
    | Case(b, e) ->
        let v = evaluate ctx e

        ValCase(b, v)
    | Recursive(v, e1, e2) ->
        let rec le = lazy evaluate (Map.add v le ctx) e1
        let rctx = Map.add v le ctx

        evaluate rctx e2
    | Unit -> ValUnit


// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Ultimate functional programming - lists and List.map!
// We represent lists as cons cells using tuples, so [1,2,3]
//
// = Case(true, Tuple(Constant(1), Case(true, Tuple(Constant(2),
//     Case(true, Tuple(Constant(3), Case(false, Unit) ))))))

// Helper function to construct lists, so that we
// do not need to write them by hand!
let rec makeListExpr l =
    match l with
    | x :: xs -> Case(true, Tuple(x, makeListExpr xs))
    | [] -> Case(false, Unit)

let el = makeListExpr [ for i in 1..5 -> Constant i ]

// List.map function in TinyML:
//
//   let rec map = (fun f -> fun l ->
//     match l with
//     | Case1 t -> Case1(f x#1, (map f) x#2)
//     | Case2(Unit) -> Case2(Unit))
//   in map (fun y -> y * 10) l
//
let em =
    Recursive(
        "map",
        Lambda(
            "f",
            Lambda(
                "l",
                Match(
                    Variable("l"),
                    "x",
                    Case(
                        true,
                        Tuple(
                            Application(Variable "f", TupleGet(true, Variable "x")),
                            Application(Application(Variable "map", Variable "f"), TupleGet(false, Variable "x"))
                        )
                    ),
                    Case(false, Unit)
                )
            )
        ),
        Application(Application(Variable "map", Lambda("y", Binary("*", Variable "y", Constant 10))), el)
    )

evaluate Map.empty em

// TODO: Can you implement 'List.filter' in TinyML too??
// The somewhat silly example removes 3 from the list.
// Add '%' binary operator and you can remove odd/even numbers!
//
//   let rec filter = (fun f -> fun l ->
//     match l with
//     | Case1 t ->
//          if f x#1 then Case1(x#1, (filter f) x#2)
//          else (filter f) x#2
//     | Case2(Unit) -> Case2(Unit))
//   in filter (fun y -> y + (-2)) l
//
let ef =
    Recursive(
        "filter",
        Lambda(
            "f",
            Lambda(
                "l",
                Match(
                    Variable("l"),
                    "x",
                    If(
                        Application(Variable "f", TupleGet(true, Variable "x")),
                        Case(
                            true,
                            Tuple(
                                TupleGet(true, Variable "x"),
                                Application(Application(Variable "filter", Variable "f"), TupleGet(false, Variable "x"))
                            )
                        ),
                        Application(Application(Variable "filter", Variable "f"), TupleGet(false, Variable "x"))
                    ),
                    Case(false, Unit)
                )
            )
        ),
        Application(Application(Variable "filter", Lambda("y", Binary("+", Variable "y", Constant -2))), el)
    )

evaluate Map.empty ef
