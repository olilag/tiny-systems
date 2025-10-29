// ----------------------------------------------------------------------------
// 03 - Type inference for binary operators and conditionals
// ----------------------------------------------------------------------------

// NOTE: Start with some basic expressions from TinyML
// This time, If requires a real Boolean argument and we have
// operators '+' (int -> int -> int) and '=' (int -> int -> bool)
type Expression =
    | Constant of int
    | Binary of string * Expression * Expression
    | If of Expression * Expression * Expression
    | Variable of string

type Type =
    | TyVariable of string
    | TyBool
    | TyNumber
    | TyList of Type

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------

let rec occursCheck vcheck ty =
    match ty with
    | TyVariable name -> name = vcheck
    | TyBool
    | TyNumber -> false
    | TyList ty -> occursCheck vcheck ty

let rec substType (subst: Map<string, Type>) ty =
    match ty with
    | TyVariable name ->
        match Map.tryFind name subst with
        | Some s -> s
        | None -> TyVariable name
    | TyBool -> TyBool
    | TyNumber -> TyNumber
    | TyList ty -> TyList(substType subst ty)

let substConstrs (subst: Map<string, Type>) (cs: list<Type * Type>) =
    cs |> List.map (fun (tyl, tyr) -> substType subst tyl, substType subst tyr)


let rec solve cs =
    match cs with
    | [] -> []
    | (TyNumber, TyNumber) :: cs
    | (TyBool, TyBool) :: cs -> solve cs
    | (TyList ty1, TyList ty2) :: cs -> solve ((ty1, ty2) :: cs)
    | (ty, TyVariable v) :: cs
    | (TyVariable v, ty) :: cs ->
        if occursCheck v ty then
            failwith "Cannot be solved (occurs check)"

        let subst = Map.ofList [ v, ty ]
        let cs = substConstrs subst cs
        let subst = solve cs
        let ty = substType (Map.ofList subst) ty
        (v, ty) :: subst
    | (TyNumber, _) :: _
    | (TyBool, _) :: _
    | (TyList _, _) :: _
    | (_, TyNumber) :: _
    | (_, TyBool) :: _
    | (_, TyList _) :: _ -> failwith "Cannot be solved"

// ----------------------------------------------------------------------------
// Constraint generation & inference
// ----------------------------------------------------------------------------

// Variable context to keep types of declared variables
// (those will typically be TyVariable cases, but don't have to)
type TypingContext = Map<string, Type>

let rec generate (ctx: TypingContext) e =
    match e with
    | Constant _ ->
        // NOTE: If the expression is a constant number, we return
        // its type (number) and generate no further constraints.
        TyNumber, []

    | Binary("+", e1, e2) ->
        // NOTE: Recursively process sub-expressions, collect all the
        // constraints and ensure the types of 'e1' and 'e2' are 'TyNumber'
        let t1, s1 = generate ctx e1
        let t2, s2 = generate ctx e2
        TyNumber, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]

    | Binary("=", e1, e2) ->
        // TODO: Similar to the case for '+' but returns 'TyBool'
        let t1, s1 = generate ctx e1
        let t2, s2 = generate ctx e2
        TyBool, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]

    | Binary(op, _, _) -> failwithf "Binary operator '%s' not supported." op

    | Variable v ->
        // TODO: Just get the type of the variable from 'ctx' here.
        match Map.tryFind v ctx with
        | Some ty -> ty, []
        | None -> failwithf "Variable '%s' not in context." v

    | If(econd, etrue, efalse) ->
        // TODO: Call generate recursively on all three sub-expressions,
        // collect all constraints and add a constraint that (i) the type
        // of 'econd' is 'TyBool' and (ii) types of 'etrue' and 'efalse' match.
        let cty, s1 = generate ctx econd
        let tty, s2 = generate ctx etrue
        let fty, s3 = generate ctx efalse
        tty, s1 @ s2 @ s3 @ [ cty, TyBool; tty, fty ]


// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------


// Simple expressions: x = 10 + x
// Assuming x:'a, infers that 'a = int
let e1 = Binary("=", Variable("x"), Binary("+", Constant(10), Variable("x")))

let t1, cs1 = generate (Map.ofList [ "x", TyVariable "a" ]) e1

solve cs1

// Simple expressions: if x then 2 + 1 else y
// Assuming x:'a, y:'b, infers 'a = bool, 'b = int
let e2 = If(Variable("x"), Binary("+", Constant(2), Constant(1)), Variable("y"))

let t2, cs2 = generate (Map.ofList [ "x", TyVariable "a"; "y", TyVariable "b" ]) e2

solve cs2

// Simple expressions: if x then 2 + 1 else x
// Cannot be solved, because 'x' used as 'int' and 'bool'
let e3 = If(Variable("x"), Binary("+", Constant(2), Constant(1)), Variable("x"))

let t3, cs3 = generate (Map.ofList [ "x", TyVariable "a"; "y", TyVariable "b" ]) e3

solve cs3
