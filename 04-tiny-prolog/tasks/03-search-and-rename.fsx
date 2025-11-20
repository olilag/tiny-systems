// ----------------------------------------------------------------------------
// 03 - Searching for clauses & variable renaming
// ----------------------------------------------------------------------------

type Term =
    | Atom of string
    | Variable of string
    | Predicate of string * Term list

type Clause = { Head: Term; Body: Term list }

type Program = Clause list

let fact p = { Head = p; Body = [] }

let rule p b = { Head = p; Body = b }

// ----------------------------------------------------------------------------
// Substitutions and unification of terms
// ----------------------------------------------------------------------------

let rec substitute (subst: Map<string, Term>) term =
    // Replace variables in 'term' for which there is a
    // replacement specified by 'subst.[var]' with the replacement.
    // You can assume the terms in 'subst' do not contain
    // any of the variables that we want to replace.
    match term with
    | Atom a -> Atom a
    | Variable v ->
        match Map.tryFind v subst with
        | Some s -> s
        | None -> Variable v
    | Predicate(p, ts) ->
        let ts = ts |> List.map (fun t -> substitute subst t)
        Predicate(p, ts)


let substituteSubst (newSubst: Map<string, Term>) (subst: list<string * Term>) =
    // Apply the substitution 'newSubst' to all the terms
    // in the existing substitiution 'subst'. (We represent one
    // as a map and the other as a list of pairs, which is a bit
    // inelegant, but it makes calling this function easier later.)
    subst |> List.map (fun (s, t) -> s, substitute newSubst t)


let substituteTerms (subst: Map<string, Term>) (terms: list<Term>) =
    // Apply substitution 'subst' to all the terms in 'terms'
    terms |> List.map (fun t -> substitute subst t)


let rec unifyLists l1 l2 =
    match l1, l2 with
    | [], [] -> Some []
    | h1 :: t1, h2 :: t2 ->
        let s1 = unify h1 h2

        match s1 with
        | Some s1 ->
            let subst = Map.ofList s1
            let t1 = substituteTerms subst t1
            let t2 = substituteTerms subst t2

            match unifyLists t1 t2 with
            | Some s2 ->
                let subst = Map.ofList s2
                let s1 = substituteSubst subst s1
                Some(s1 @ s2)
            | _ -> None
        | _ -> None
    | _ -> None

and unify t1 t2 : option<list<string * Term>> =
    match t1, t2 with
    | Atom a1, Atom a2 when a1 = a2 -> Some []
    | Predicate(p1, t1), Predicate(p2, t2) when p1 = p2 -> unifyLists t1 t2
    | Variable v, t
    | t, Variable v -> Some [ v, t ]
    | _ -> None

// ----------------------------------------------------------------------------
// Searching the program (database) and variable renaming
// ----------------------------------------------------------------------------

let nextNumber =
    let mutable n = 0

    fun () ->
        n <- n + 1
        n

let rec freeVariables term =
    // Return a list of all variables that appear in 'term'
    // (this may contain duplicates, we will eliminate them below)
    // HINT: Use List.collect: ('a -> list<'b>) -> list<'a> -> list<'b>
    match term with
    | Atom _ -> []
    | Variable v -> [ v ]
    | Predicate(_, ts) -> ts |> List.collect freeVariables


let withFreshVariables (clause: Clause) : Clause =
    // Get a list of distinct variables in the clause (using
    // 'freeVariables' and 'List.distinct'), generate a substitution
    // that append a number 'n' obtained by 'nextNumber()' to the end
    // of all the variable names, and apply the substitutions to the
    // head and body of the clause.
    //
    // For example, 'grandparent(X,Y) :- parent(X,Z), parent(Z,Y)' may
    // become 'grandparent(X3,Y3) :- parent(X3,Z3), parent(Z3,Y3)'
    //
    // This may not be correct if the user-provided names of variables
    // had numbers in them in a certain format, but that's OK for now!
    let vars =
        freeVariables clause.Head @ (clause.Body |> List.collect freeVariables)
        |> List.distinct

    let n = nextNumber ()
    let subst = Map.ofList (vars |> List.map (fun v -> v, Variable(sprintf "%s%i" v n)))
    let head = substitute subst clause.Head
    let body = substituteTerms subst clause.Body
    { Head = head; Body = body }


let query (program: list<Clause>) (query: Term) : list<Clause * list<string * Term>> =
    // Return all clauses from 'program' whose 'Head' can be
    // unified with the specified 'query' and return the resulting
    // substitutions. Before unifying, rename variables in the program
    // rule using 'withFreshVariables'. You can do this using 'List.choose'
    // or by using list comprehension.
    //
    // The return type of this is a list of tuples consisting of the matching
    // clause and a substitution (list<string * Term>). Calling 'unify'
    // gives you 'option<list<string * Term>>', so you need to pattern match
    // on this and if it is 'Some(subst)' return 'Some(clause, subst)'.
    program
    |> List.choose (fun c ->
        let c = withFreshVariables c
        Option.bind (fun subst -> Some(c, subst)) (unify c.Head query))


// ----------------------------------------------------------------------------
// Querying the British royal family
// ----------------------------------------------------------------------------

// Generating fresh variables - repeated calls
// should append new number to all variable names
rule
    (Predicate("grandparent", [ Variable("X"); Variable("Y") ]))
    [ Predicate("parent", [ Variable("X"); Variable("Z") ])
      Predicate("parent", [ Variable("Z"); Variable("Y") ]) ]
|> withFreshVariables

// Some information about the British royal family
let family =
    [ fact (Predicate("male", [ Atom("William") ]))
      fact (Predicate("female", [ Atom("Diana") ]))
      fact (Predicate("male", [ Atom("Charles") ]))
      fact (Predicate("male", [ Atom("George") ]))
      fact (Predicate("parent", [ Atom("Diana"); Atom("William") ]))
      fact (Predicate("parent", [ Atom("Charles"); Atom("William") ]))
      fact (Predicate("parent", [ Atom("William"); Atom("George") ]))
      rule
          (Predicate("father", [ Variable("X"); Variable("Y") ]))
          [ Predicate("parent", [ Variable("X"); Variable("Y") ])
            Predicate("male", [ Variable("X") ]) ] ]

// Query: male(X)
// Match #1: male(William)
// Match #2: male(Charles)
// Match #3: male(George)
query family (Predicate("male", [ Variable("X") ]))

// Query: father(X, William)
// Match #1: father(X, Y) :- parent(X, Y), male(X)
query family (Predicate("father", [ Variable("X"); Atom("William") ]))
