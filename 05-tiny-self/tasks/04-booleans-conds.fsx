// ----------------------------------------------------------------------------
// 04 - Implementing booleans and passing blocks as arguments
// ----------------------------------------------------------------------------

type Slot =
    { Name: string
      Contents: Objekt
      IsParent: bool }

and Objekt =
    { mutable Slots: Slot list
      mutable Code: Objekt option
      mutable Special: Special option }

and Special =
    | String of string
    | Native of (Objekt -> Objekt)

#load "objekt-visualizer.fs"
open TinySelf

// ----------------------------------------------------------------------------
// Helpers for creating things that we will often need
// ----------------------------------------------------------------------------

let makeCodeObject slots code =
    { Code = Some code
      Special = None
      Slots = slots }

let makeObject slots =
    { Code = None
      Special = None
      Slots = slots }

let makeSpecialObject special =
    { Code = None
      Special = Some special
      Slots = [] }

let makeSlot (n: string) contents =
    if n.EndsWith("*") then
        failwith "Non-parent slot names should not end with '*'."

    { Name = n
      Contents = contents
      IsParent = false }

let makeParentSlot (n: string) contents =
    if not (n.EndsWith("*")) then
        failwith "Parent slot names should end with '*'."

    { Name = n
      Contents = contents
      IsParent = true }

let makeNativeMethod f =
    makeCodeObject [] (makeSpecialObject (Native(f)))

// ----------------------------------------------------------------------------
// Lookup and message sending
// ----------------------------------------------------------------------------

let rec lookup (msg: string) (obj: Objekt) : list<Objekt * Slot> =
    match obj.Slots |> List.tryFind (fun s -> s.Name = msg) with
    | Some slot -> [ obj, slot ]
    | None ->
        obj.Slots
        |> List.filter (fun s -> s.IsParent)
        |> List.map (fun s -> s.Contents)
        |> List.collect (fun o -> lookup msg o)

let callMethod (method: Objekt) (args: Objekt) (instance: Objekt) : Objekt =
    let arcd =
        { method with
            Slots =
                method.Slots
                @ [ makeParentSlot "args*" args; makeParentSlot "receiver*" instance ] }

    match method with
    | { Special = Some(Native f) } -> f arcd
    | _ -> failwith "not implemented"

let eval (slotValue: Objekt) (args: Objekt) (instance: Objekt) : Objekt =
    match slotValue with
    | { Code = None } -> slotValue
    | { Code = Some method } -> callMethod method args instance

let send (msg: string) (args: Objekt) (instance: Objekt) : Objekt =
    match lookup msg instance with
    | [ _, s ] -> eval s.Contents args instance
    | [] -> failwithf "No slot named %s" msg
    | _ -> failwithf "Multiple slots named %s" msg

// ----------------------------------------------------------------------------
// Helpers for testing & object construction
// ----------------------------------------------------------------------------

let empty: Objekt = makeObject []

let getStringValue (obj: Objekt) : string =
    let sObj = send "value" empty obj

    match sObj.Special with
    | Some(String s) -> s
    | _ -> failwith "Object is not a string"

let printCode: Objekt =
    makeNativeMethod (fun arcd ->
        printfn "%s" (getStringValue arcd)
        empty)

// ----------------------------------------------------------------------------
// Assignment slots
// ----------------------------------------------------------------------------

let assignmentMethod n =
    makeNativeMethod (fun arcd ->
        match lookup n arcd with
        | [ n, s ] ->
            let newVal =
                match lookup "new" arcd with
                | [ _, slot ] -> slot.Contents
                | [] -> failwith "No slot named new"
                | _ -> failwith "Multiple slots named new"

            let newSlots =
                n.Slots
                |> List.map (fun item ->
                    if item.Name = s.Name then
                        { item with Contents = newVal }
                    else
                        item)

            n.Slots <- newSlots
            n
        | [] -> failwithf "No slot named %s" n
        | _ -> failwithf "Multiple slots named %s" n)

let makeAssignmentSlot n =
    { Name = n + ":"
      Contents = assignmentMethod n
      IsParent = false }

// ----------------------------------------------------------------------------
// Primitive types - Booleans, strings and blocks
// ----------------------------------------------------------------------------


// Implement a helper function that creates (the two) boolean objects.
// Boolean is an object with 'if' method. The method takes two parameters
// called 'then' and 'else'. They are blocks (created using 'makeBlock' -
// see below). The method runs the correct block (depending on whether
// 'b' is true or false) by sending it 'run' message with empty arguments.

let makeBoolean b =
    makeObject
        [ makeSlot
              "if"
              (makeNativeMethod (fun arcd ->
                  let slotName = if b then "then" else "else"
                  arcd |> send slotName empty |> send "run" empty)) ]

let trueObj = makeBoolean true
let falseObj = makeBoolean false


// Implement equality testing for strings. The 'equals' method takes
// a parameter 'other' with the other string (same as append). It should
// return 'trueObj' if the two strings are equal or 'falseObj' otherwise.

let equalsCode =
    makeNativeMethod (fun arcd ->
        let first = arcd |> getStringValue
        let second = arcd |> send "other" empty |> getStringValue

        if first = second then trueObj else falseObj)

let rec appendCode =
    makeNativeMethod (fun arcd ->
        let first = arcd |> getStringValue
        let second = arcd |> send "other" empty |> getStringValue

        makeString (first + second))

and stringPrototype =
    makeObject
        [ makeSlot "print" printCode
          makeSlot "append" appendCode
          makeSlot "equals" equalsCode ]

and makeString s =
    makeObject
        [ makeSlot "value" (makeSpecialObject (String s))
          makeParentSlot "string*" stringPrototype ]

// DEMO: Helper function to create a block object. This is just
// an object with 'run' method that runs some native F# code.

let makeBlock f =
    makeObject [ makeSlot "run" (makeNativeMethod (fun _ -> f ())) ]

// ----------------------------------------------------------------------------
// Prisoner's dilemma
// ----------------------------------------------------------------------------

// DEMO: Implementing Prisoner's dilemma. We have two players who
// can choose to betray/cooperate. Based on their choices, we want
// to report who spends how many years in prison. For the rules, see:
// https://en.wikipedia.org/wiki/Prisoner%27s_dilemma

let betray = makeString "betray"
let coop = makeString "cooperate"

let rnd = System.Random()
let player1 = if rnd.Next(2) = 0 then betray else coop
let player2 = if rnd.Next(2) = 0 then betray else coop

// NOTE: Variables 'player1' and 'player2' are just Self strings.
// This tests if player1 = "cooperate" - it returns a string
// "then clause" or "else clause" and then prints the result.
//
// Modify the code to test for the possible player1/player2 combinations!
// The easiest option is to model a nested if - something like:
//
//   if player1 = "cooperate" then
//     if player2 = "cooperate" then "cooperate-cooperate: each serves 1 year"
//     else "cooperate-betray: #1 gets 3 years, #2 is free"
//   else
//     if player2 = "cooperate" then "betray-cooperate: #1 is free, #2 gets 3 years"
//     else "betray-betray: each serves 2 years"
//
player1
|> send "equals" (makeObject [ makeSlot "other" coop ])
|> send
    "if"
    (makeObject
        [ makeSlot
              "then"
              (makeBlock (fun () ->
                  player2
                  |> send "equals" (makeObject [ makeSlot "other" coop ])
                  |> send
                      "if"
                      (makeObject
                          [ makeSlot
                                "then"
                                (makeBlock (fun () -> makeString "cooperate-cooperate: each serves 1 year"))
                            makeSlot
                                "else"
                                (makeBlock (fun () -> makeString "cooperate-betray: #1 gets 3 years, #2 is free")) ])))
          makeSlot
              "else"
              (makeBlock (fun () ->
                  player2
                  |> send "equals" (makeObject [ makeSlot "other" coop ])
                  |> send
                      "if"
                      (makeObject
                          [ makeSlot
                                "then"
                                (makeBlock (fun () -> makeString "betray-cooperate: #1 is free, #2 gets 3 years"))
                            makeSlot "else" (makeBlock (fun () -> makeString "betray-betray: each serves 2 years")) ]))) ])
|> send "print" empty
