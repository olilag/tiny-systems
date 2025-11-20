// ----------------------------------------------------------------------------
// 07 - Generating magic squares in TinyProlog
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
// Pretty printing terms
// ----------------------------------------------------------------------------

let rec (|Number|_|) term =
    // Write an active pattern to recognize numbers in the form used below.
    // If the term is 'Atom("zero")' return Some(0).
    // If the term is 'Predicate("succ", [n])' where 'n' is itself
    // a term representing number, return the number value +1.
    match term with
    | Atom "zero" -> Some 0
    | Predicate("succ", [ Number n ]) -> Some(n + 1)
    | _ -> None


let rec (|List|_|) term : option<list<Term>> =
    // If the term represents a list, this should return the
    // elements of the list collected in an ordinary F# list.
    // If the term is 'Atom("empty")' return Some([])
    // If the term is 'Predicate("cons", [h; tl])' where 'tl' is itself
    // a term representing a list 'l', return Some(h::l).
    match term with
    | Atom "empty" -> Some []
    | Predicate("cons", [ h; List l ]) -> Some(h :: l)
    | _ -> None



let rec formatTerm term =
    match term with
    | Number n -> string n
    | List l -> sprintf "[%s]" (l |> List.map formatTerm |> String.concat ", ")
    | Atom s -> s
    | Variable v -> v
    | Predicate(p, items) -> sprintf "%s(%s)" p (items |> List.map formatTerm |> String.concat ", ")

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


let rec solve program subst goals : seq<list<string * Term>> =
    seq {
        // We want to change this function to return a lazy sequence
        // of all possible substitutions solving the problem. I already
        // wrapped the code in 'seq { .. }' block for you. Change the rest
        // to recursively call 'solve' using 'yield!' and return new
        // solutions using 'yield' (replacing the printing code).
        match goals with
        | g :: goals ->
            // We need to solve the goal (term) 'g'. To do so, find all
            // matching clauses in the 'program' using 'query' and iterate over
            // the returned list using 'for clause, newSubst in matches do'.
            // For each possible solution, we need to add the 'clause.Body' to
            // the list of 'goals' and apply the substitution 'newSubst' to the
            // new concatentated list of 'goals'. Then we need to apply the
            // substitution 'newSubst' to the substitution 'subst' we have so far,
            // append the two and call 'solve' recursively with this new substitution
            // to solve the new goals.
            let matches = query program g

            for clause, newSubst in matches do
                let newGoals = substituteTerms (Map.ofList newSubst) (goals @ clause.Body)
                let subst = substituteSubst (Map.ofList newSubst) subst
                yield! solve program (subst @ newSubst) newGoals

        | [] ->
            // We solved all goals, which means 'subst' is a possible solution!
            yield subst

    }


let run program query =
    let vars = Set.ofSeq (freeVariables query)

    for subst in solve program [] [ query ] do
        // To avoid cluttered output, we want to only print assignment
        // for variables that appear in the original query (and skip all
        // variables generated by the various internal matches). You can do
        // this here by iterating over variables and printing them only if
        // they are included in 'vars' (test using 'vars.Contains')
        for (v, t) in subst |> List.filter (fun (v, t) -> vars.Contains v) do
            printfn "%s -> %s" v (formatTerm t)

        printfn "--------------------------------------------"

// ----------------------------------------------------------------------------
// Calculating with numbers
// ----------------------------------------------------------------------------

let rec num n =
    // Write a helper that generates a term representing number.
    // This should return Atom("zero") when n is 0 and otherwise
    // succ(succ(...(zero))) with appropriate number of 'succ's.
    match n with
    | 0 -> Atom "zero"
    | n -> Predicate("succ", [ num (n - 1) ])

let nums =
    [ fact (Predicate("add", [ Atom("zero"); Variable("X"); Variable("X") ]))
      rule
          (Predicate(
              "add",
              [ Predicate("succ", [ Variable("X") ])
                Variable("Y")
                Predicate("succ", [ Variable("Z") ]) ]
          ))
          [ Predicate("add", [ Variable("X"); Variable("Y"); Variable("Z") ]) ]
      fact (Predicate("eq", [ Variable("X"); Variable("X") ])) ]


// ----------------------------------------------------------------------------
// Working with lists
// ----------------------------------------------------------------------------

let rec makeList l : Term =
    // Write a helper that generates a term representing a list.
    // This should return Atom("empty") when 'l' is [] and otherwise
    // cons(t1, .. cons(tN, empty)) when 'l' is [t1; ...; tN]
    match l with
    | [] -> Atom "empty"
    | h :: t -> Predicate("cons", [ h; makeList t ])

let append =
    [ fact (Predicate("append", [ Atom("empty"); Variable("X"); Variable("X") ]))
      rule
          (Predicate(
              "append",
              [ Predicate("cons", [ Variable("X"); Variable("Y") ])
                Variable("Z")
                Predicate("cons", [ Variable("X"); Variable("W") ]) ]
          ))
          [ Predicate("append", [ Variable("Y"); Variable("Z"); Variable("W") ]) ] ]

let l1to4 = makeList [ for i in 1..4 -> num i ]
let l5to9 = makeList [ for i in 5..9 -> num i ]
let l1to9 = makeList [ for i in 1..9 -> num i ]

let permutation =
    append
    @ [ fact (Predicate("perm", [ Atom("empty"); Atom("empty") ]))
        rule
            (Predicate("perm", [ Variable("L"); Predicate("cons", [ Variable("H"); Variable("T") ]) ]))
            [ Predicate(
                  "append",
                  [ Variable("V")
                    Predicate("cons", [ Variable("H"); Variable("U") ])
                    Variable("L") ]
              )
              Predicate("append", [ Variable("V"); Variable("U"); Variable("W") ])
              Predicate("perm", [ Variable("W"); Variable("T") ]) ] ]

// DEMO: Generate all permutations of the list [1 .. 4]
run permutation (Predicate("perm", [ l1to4; Variable("X") ]))


// ----------------------------------------------------------------------------
// Generating magic squares
// ----------------------------------------------------------------------------

// Custom operator and a hlper function for equality & defining variables
let (.=.) a b = Predicate("eq", [ a; b ])
let var x = Variable(x)

// TinyProlog is too slow! But if we give it the numbers in an order
// that is close to being a magic square (first row is correct), it will
// manage to generate a magic square sooner or later...
let l = [ 2; 7; 6; 1; 3; 4; 5; 8; 9 ]

let magic =
    permutation
    @ nums
    @ [ rule
            (Predicate("add3", [ var "A"; var "B"; var "C"; var "S" ]))
            [ Predicate("add", [ var "A"; var "B"; var "T" ])
              Predicate("add", [ var "T"; var "C"; var "S" ]) ]
        rule
            (Predicate("magic", [ var "S"; var "X" ]))
            [ yield Predicate("perm", [ makeList [ for i in l -> num i ]; var "X" ])
              yield
                  var "X"
                  .=. makeList
                          [ var "A1"
                            var "A2"
                            var "A3"
                            var "B1"
                            var "B2"
                            var "B3"
                            var "C1"
                            var "C2"
                            var "C3" ]
              for a, b, c in
                  [ ("A1", "A2", "A3")
                    ("B1", "B2", "B3")
                    ("C1", "C2", "C3")
                    ("A1", "B1", "C1")
                    ("A2", "B2", "C2")
                    ("A3", "B3", "C3")
                    ("A1", "B2", "C3")
                    ("A3", "B2", "C1") ] do
                  yield Predicate("add3", [ var a; var b; var c; var "S" ]) ] ]

run magic (Predicate("magic", [ num 15; var "X" ]))
