// ----------------------------------------------------------------------------
// Type inference for binary operators and conditionals
// ----------------------------------------------------------------------------

type Expression =
    | Constant of int
    | Binary of string * Expression * Expression
    | If of Expression * Expression * Expression
    | Variable of string
    // NOTE: Added three more kinds of expression from TinyML
    | Application of Expression * Expression
    | Lambda of string * Expression
    | Let of string * Expression * Expression

type Type =
    | TyVariable of string
    | TyBool
    | TyNumber
    | TyList of Type
    // NOTE: Added type for functions (of single argument)
    | TyFunction of Type * Type

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------

let rec occursCheck vcheck ty =
    // TODO: Add case for 'TyFunction' (need to check both nested types)
    match ty with
    | TyVariable name -> name = vcheck
    | TyBool
    | TyNumber -> false
    | TyList ty -> occursCheck vcheck ty
    | TyFunction(arg, ret) -> occursCheck vcheck arg || occursCheck vcheck ret

let rec substType (subst: Map<_, _>) t1 =
    // TODO: Add case for 'TyFunction' (need to substitute in both nested types)
    match t1 with
    | TyVariable name ->
        match Map.tryFind name subst with
        | Some s -> s
        | None -> TyVariable name
    | TyBool -> TyBool
    | TyNumber -> TyNumber
    | TyList ty -> TyList(substType subst ty)
    | TyFunction(arg, ret) -> TyFunction(substType subst arg, substType subst ret)

let substConstrs subst cs =
    cs |> List.map (fun (tyl, tyr) -> substType subst tyl, substType subst tyr)

let rec solve constraints =
    // TODO: Add case matching TyFunction(ta1, tb1) and TyFunction(ta2, tb2)
    // This generates two new constraints, equating the argument/return types.
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
    | (TyFunction(ta1, tb1), TyFunction(ta2, tb2)) :: cs -> solve ([ ta1, ta2; tb1, tb2 ] @ cs)
    | (t1, t2) :: _ -> failwithf "Cannot be solved (%A = %A)" t1 t2

// ----------------------------------------------------------------------------
// Constraint generation & inference
// ----------------------------------------------------------------------------

type TypingContext = Map<string, Type>

// NOTE: You will need this helper in checking of Lambda and Application.
// It generates a new type variable each time you call 'newTypeVariable()'
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
        // TODO: Generate type & constraints for 'e1' first and then
        // add the generated type to the typing context for 't2'.
        let t1, s1 = generate ctx e1
        let ctx = Map.add v t1 ctx
        let t2, s2 = generate ctx e2
        t2, s1 @ s2

    | Lambda(v, e) ->
        let targ = newTyVariable ()
        // TODO: We do not know what the type of the variable 'v' is, so we
        // generate a new type variable and add that to the 'ctx'. The
        // resulting type will be 'TyFunction' with 'targ' as argument type.
        let ctx = Map.add v targ ctx
        let tret, s = generate ctx e
        TyFunction(targ, tret), s

    | Application(e1, e2) ->
        // TODO: Tricky case! We cannot inspect the generated type of 'e1'
        // to see what the argument/return type of the function is. Instead,
        // we have to generate a new type variable and add a constraint.
        let tret = newTyVariable ()
        let t1, s1 = generate ctx e1
        let t2, s2 = generate ctx e2
        tret, s1 @ s2 @ [ t1, TyFunction(t2, tret) ]


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


// NOTE: Using the above, you will end up with ugly random type variable
// names like '_a4' etc. You can improve this by collecting all the type
// variable names that appear in a type and substituting them with a
// list of nice names. Useful bit of code to generate the substitution is:
//
//   Map.ofList [ for i, n in Seq.indexed ["_a4"; "_a5"] ->
//     n, string('a' + char i) ]
//
// You would still need to write code to collect all type variables in a type.


// let x = 10 in x = 10
Let("x", Constant 10, Binary("=", Variable "x", Constant 10)) |> infer

// let f = fun x -> x*2 in (f 20) + (f 1)
Let(
    "f",
    Lambda("x", Binary("*", Variable("x"), Constant(2))),
    Binary("+", Application(Variable("f"), Constant(20)), Application(Variable("f"), Constant(1)))
)
|> infer

// fun x f -> f (f x)
Lambda("x", Lambda("f", Application(Variable "f", Application(Variable "f", Variable "x"))))
|> infer

// fun f -> f f
// This does not type check due to occurs check
Lambda("f", Application(Variable "f", Variable "f")) |> infer

// fun f -> f 1 + f (2 = 3)
// This does not type check because argument of 'f' cannot be both 'int' and 'bool'
Lambda(
    "f",
    Binary("+", Application(Variable "f", Constant 1), Application(Variable "f", Binary("=", Constant 2, Constant 3)))
)
|> infer
