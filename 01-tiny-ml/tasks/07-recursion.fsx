// ----------------------------------------------------------------------------
// 07 - Add support for recursion
// ----------------------------------------------------------------------------

type Value =
    | ValNum of int
    | ValClosure of string * Expression * VariableContext
    | ValTuple of Value * Value
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
    | Case of bool * Expression
    | Match of Expression * string * Expression * Expression
    // NOTE: A recursive definition. You can think of
    // 'Let(v, e1, e2)' as 'let rec v = e1 in e2'.
    | Recursive of string * Expression * Expression

and VariableContext =
    // NOTE: For recursive calls, we need to add the function
    // being defined to the variable context when defining it.
    // This can be done using 'let rec', but we need to store
    // the variables as lazy values.
    Map<string, Lazy<Value>>

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
        | Some res ->
            // NOTE: As 'res' is now 'Lazy<Value>' we need to get its value here.
            res.Value
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

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Recursion and conditionals - implementing factorial!
//   let rec factorial = fun x ->
//     if x then 1 else x*(factorial (-1 + x))
//   in factorial 5
let er =
    Recursive(
        "factorial",
        Lambda(
            "x",
            If(
                Variable("x"),
                Constant(1),
                Binary("*", Variable("x"), Application(Variable("factorial"), Binary("+", Constant(-1), Variable("x"))))
            )
        ),
        Application(Variable "factorial", Constant 5)
    )

evaluate Map.empty er
