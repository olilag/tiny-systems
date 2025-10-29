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
    | Tuple of Expression * Expression
    | TupleGet of bool * Expression
    | Case of bool * Expression
    | Match of Expression * string * Expression * Expression
    // NOTE: Added the unit value and recursive definition
    | Recursive of string * Expression * Expression
    | Unit

type Type =
    | TyVariable of string
    | TyBool
    | TyNumber
    | TyList of Type
    | TyFunction of Type * Type
    | TyTuple of Type * Type
    | TyUnion of Type * Type
    // NOTE: We need another primitive type for units
    | TyUnit

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------

let rec occursCheck vcheck ty =
    // TODO: Add case for 'TyUnit' (same as 'TyNumber' or 'TyBool')
    match ty with
    | TyVariable name -> name = vcheck
    | TyBool
    | TyNumber
    | TyUnit -> false
    | TyList ty -> occursCheck vcheck ty
    | TyFunction(arg, ret) -> occursCheck vcheck arg || occursCheck vcheck ret
    | TyTuple(t1, t2) -> occursCheck vcheck t1 || occursCheck vcheck t2
    | TyUnion(t1, t2) -> occursCheck vcheck t1 || occursCheck vcheck t2

let rec substType (subst: Map<_, _>) t1 =
    // TODO: Add case for 'TyUnit' (same as 'TyNumber' or 'TyBool')
    match t1 with
    | TyVariable name ->
        match Map.tryFind name subst with
        | Some s -> s
        | None -> TyVariable name
    | TyBool -> TyBool
    | TyNumber -> TyNumber
    | TyUnit -> TyUnit
    | TyList ty -> TyList(substType subst ty)
    | TyFunction(arg, ret) -> TyFunction(substType subst arg, substType subst ret)
    | TyTuple(t1, t2) -> TyTuple(substType subst t1, substType subst t2)
    | TyUnion(t1, t2) -> TyUnion(substType subst t1, substType subst t2)

let substConstrs subst cs =
    cs |> List.map (fun (tyl, tyr) -> substType subst tyl, substType subst tyr)

let rec solve constraints =
    // TODO: Add case for 'TyUnit' (same as 'TyNumber' or 'TyBool')
    match constraints with
    | [] -> []
    | (TyNumber, TyNumber) :: cs
    | (TyBool, TyBool) :: cs
    | (TyUnit, TyUnit) :: cs -> solve cs
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
    | (TyTuple(ta1, tb1), TyTuple(ta2, tb2)) :: cs
    | (TyUnion(ta1, tb1), TyUnion(ta2, tb2)) :: cs -> solve ([ ta1, ta2; tb1, tb2 ] @ cs)
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
        let t1, s1 = generate ctx e1
        let t2, s2 = generate ctx e2
        TyTuple(t1, t2), s1 @ s2
    | TupleGet(b, e) ->
        let t1 = newTyVariable ()
        let t2 = newTyVariable ()
        let ttu, s = generate ctx e
        (if b then t1 else t2), s @ [ ttu, TyTuple(t1, t2) ]
    | Match(e, v, e1, e2) ->
        let tu1 = newTyVariable ()
        let tu2 = newTyVariable ()
        let tu, s1 = generate ctx e
        let ctx = Map.add v tu1 ctx
        let t1, s2 = generate ctx e1
        let ctx = Map.add v tu2 ctx
        let t2, s3 = generate ctx e2
        t1, s1 @ s2 @ s3 @ [ tu, TyUnion(tu1, tu2); t1, t2 ]
    | Case(b, e) ->
        let tyo = newTyVariable ()
        let te, s = generate ctx e
        (if b then TyUnion(te, tyo) else TyUnion(tyo, te)), s

    | Unit ->
        // NOTE: This is so easy I wrote it for you :-)
        TyUnit, []

    | Recursive(v, e1, e2) ->
        // TODO: This is easier than evaluation. We need a new type variable
        // for the type of the thing we are defining (variable 'v') and add
        // it to the context when checking both 'e1' and 'e2'.
        let trec = newTyVariable ()
        let ctx = Map.add v trec ctx
        let t1, s1 = generate ctx e1
        let t2, s2 = generate ctx e2
        t2, s1 @ s2

// ----------------------------------------------------------------------------
// Putting it together & test cases
// ----------------------------------------------------------------------------

let rec collectTypeVars ty =
    match ty with
    | TyVariable name -> [ name ]
    | TyBool
    | TyNumber
    | TyUnit -> []
    | TyList ty -> collectTypeVars ty
    | TyFunction(targ, tret) -> collectTypeVars targ @ collectTypeVars tret
    | TyTuple(t1, t2) -> collectTypeVars t1 @ collectTypeVars t2
    | TyUnion(t1, t2) -> collectTypeVars t1 @ collectTypeVars t2

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

// Helper to generate list 1 .. 5 from TinyML tasks
let rec makeListExpr l =
    match l with
    | x :: xs -> Case(true, Tuple(x, makeListExpr xs))
    | [] -> Case(false, Unit)

// We can type check this, but the type is horrible!
makeListExpr [ for i in 1..5 -> Constant i ] |> infer

// Code for the List.map function from TinyML task. This fails to check.
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
    Variable("map")
)
|> infer
