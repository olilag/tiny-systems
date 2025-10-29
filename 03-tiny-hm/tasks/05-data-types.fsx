// ----------------------------------------------------------------------------
// Adding simple data types
// ----------------------------------------------------------------------------

type Expression =
    | Constant of int
    | Binary of string * Expression * Expression
    | If of Expression * Expression * Expression
    | Variable of string
    | Application of Expression * Expression
    | Lambda of string * Expression
    | Let of string * Expression * Expression
    // NOTE: Added two types of expression for working with tuples
    | Tuple of Expression * Expression
    | TupleGet of bool * Expression

type Type =
    | TyVariable of string
    | TyBool
    | TyNumber
    | TyList of Type
    | TyFunction of Type * Type
    // NOTE: Added type for tuples
    | TyTuple of Type * Type

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------

let rec occursCheck vcheck ty =
    // TODO: Add case for 'TyTuple' (same as 'TyFunction')
    match ty with
    | TyVariable name -> name = vcheck
    | TyBool
    | TyNumber -> false
    | TyList ty -> occursCheck vcheck ty
    | TyFunction(arg, ret) -> occursCheck vcheck arg || occursCheck vcheck ret
    | TyTuple(t1, t2) -> occursCheck vcheck t1 || occursCheck vcheck t2

let rec substType (subst: Map<_, _>) t1 =
    // TODO: Add case for 'TyTuple' (same as 'TyFunction')
    match t1 with
    | TyVariable name ->
        match Map.tryFind name subst with
        | Some s -> s
        | None -> TyVariable name
    | TyBool -> TyBool
    | TyNumber -> TyNumber
    | TyList ty -> TyList(substType subst ty)
    | TyFunction(arg, ret) -> TyFunction(substType subst arg, substType subst ret)
    | TyTuple(t1, t2) -> TyTuple(substType subst t1, substType subst t2)

let substConstrs subst cs =
    cs |> List.map (fun (tyl, tyr) -> substType subst tyl, substType subst tyr)

let rec solve constraints =
    // TODO: Add case for 'TyTuple' (same as 'TyFunction')
    match constraints with
    | [] -> []
    | (TyNumber, TyNumber) :: cs
    | (TyBool, TyBool) :: cs -> solve cs
    | (TyList ty1, TyList ty2) :: cs -> solve ((ty1, ty2) :: cs)
    | (ty, TyVariable v) :: cs
    | (TyVariable v, ty) :: cs ->
        if occursCheck v ty then
            failwithf "Cannot be solved (%s occurs in %A)" v ty

        let subst = Map.ofList [ v, ty ]
        let cs = substConstrs subst cs
        let subst = solve cs
        let ty = substType (Map.ofList subst) ty
        (v, ty) :: subst
    | (TyFunction(ta1, tb1), TyFunction(ta2, tb2)) :: cs
    | (TyTuple(ta1, tb1), TyTuple(ta2, tb2)) :: cs -> solve ([ ta1, ta2; tb1, tb2 ] @ cs)
    | (t1, t2) :: _ -> failwithf "Cannot be solved (%A = %A)" t1 t2


// ----------------------------------------------------------------------------
// Constraint generation & inference
// ----------------------------------------------------------------------------

type TypingContext = Map<string, Type>

let newTyVariable =
    let mutable n = 0

    fun () ->
        n <- n + 1
        TyVariable(sprintf "_a%d" n)

let rec generate (ctx: TypingContext) e =
    match e with
    | Constant _ -> TyNumber, []
    | Binary("+", e1, e2)
    | Binary("*", e1, e2) ->
        let t1, s1 = generate ctx e1
        let t2, s2 = generate ctx e2
        TyNumber, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]
    | Binary("=", e1, e2) ->
        let t1, s1 = generate ctx e1
        let t2, s2 = generate ctx e2
        TyBool, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]
    | Binary(op, _, _) -> failwithf "Binary operator '%s' not supported." op
    | Variable v ->
        match Map.tryFind v ctx with
        | Some ty -> ty, []
        | None -> failwithf "Variable '%s' not in context." v
    | If(econd, etrue, efalse) ->
        let cty, s1 = generate ctx econd
        let tty, s2 = generate ctx etrue
        let fty, s3 = generate ctx efalse
        tty, s1 @ s2 @ s3 @ [ cty, TyBool; tty, fty ]
    | Let(v, e1, e2) ->
        let t1, s1 = generate ctx e1
        let ctx = Map.add v t1 ctx
        let t2, s2 = generate ctx e2
        t2, s1 @ s2
    | Lambda(v, e) ->
        let targ = newTyVariable ()
        let ctx = Map.add v targ ctx
        let tret, s = generate ctx e
        TyFunction(targ, tret), s
    | Application(e1, e2) ->
        let tret = newTyVariable ()
        let t1, s1 = generate ctx e1
        let t2, s2 = generate ctx e2
        tret, s1 @ s2 @ [ t1, TyFunction(t2, tret) ]

    | Tuple(e1, e2) ->
        // TODO: Easy. The returned type is composed of the types of 'e1' and 'e2'.
        let t1, s1 = generate ctx e1
        let t2, s2 = generate ctx e2
        TyTuple(t1, t2), s1 @ s2

    | TupleGet(b, e) ->
        // TODO: Trickier. The type of 'e' is some tuple, but we do not know what.
        // We need to generate two new type variables and a constraint.
        let t1 = newTyVariable ()
        let t2 = newTyVariable ()
        let ttu, s = generate ctx e
        (if b then t1 else t2), s @ [ ttu, TyTuple(t1, t2) ]

// ----------------------------------------------------------------------------
// Putting it together & test cases
// ----------------------------------------------------------------------------

let rec collectTypeVars ty =
    match ty with
    | TyVariable name -> [ name ]
    | TyBool
    | TyNumber -> []
    | TyList ty -> collectTypeVars ty
    | TyFunction(targ, tret) -> collectTypeVars targ @ collectTypeVars tret
    | TyTuple(t1, t2) -> collectTypeVars t1 @ collectTypeVars t2

let prettify ty =
    let tvars = collectTypeVars ty |> List.distinct

    let subst =
        Map.ofList [ for i, n in Seq.indexed tvars -> n, TyVariable(string ('a' + char i)) ]

    substType subst ty

// Run both of the phases and return the resulting type
let infer e =
    let typ, constraints = generate Map.empty e
    let subst = solve constraints
    let typ = substType (Map.ofList subst) typ
    let typ = prettify typ
    typ

// Basic tuple examples:
// * (2 = 21, 123)
// * (2 = 21, 123)#1
// * (2 = 21, 123)#2
let etup = Tuple(Binary("=", Constant(2), Constant(21)), Constant(123))
etup |> infer
TupleGet(true, etup) |> infer
TupleGet(false, etup) |> infer

// Interesting case with a nested tuple ('a * ('b * 'c) -> 'a * 'b)
// * fun x -> x#1, x#2#1
Lambda("x", Tuple(TupleGet(true, Variable "x"), TupleGet(true, TupleGet(false, Variable "x"))))
|> infer

// Does not type check - 'int' is not a tuple!
// * (1+2)#1
TupleGet(true, Binary("+", Constant 1, Constant 2)) |> infer


// Combining functions and tuples ('b -> (('b -> 'a) -> ('b * 'a)))
// * fun x f -> (x, f x)
Lambda("x", Lambda("f", Tuple(Variable "x", Application(Variable "f", Variable "x"))))
|> infer
