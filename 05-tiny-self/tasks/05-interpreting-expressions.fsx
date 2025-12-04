// ----------------------------------------------------------------------------
// 05 - Representing and interpreting expressions
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

let makeBoolean b =
    makeObject
        [ makeSlot
              "if"
              (makeNativeMethod (fun arcd ->
                  let slotName = if b then "then" else "else"
                  arcd |> send slotName empty |> send "run" empty)) ]

let trueObj = makeBoolean true
let falseObj = makeBoolean false

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

let makeBlock f =
    makeObject [ makeSlot "run" (makeNativeMethod (fun _ -> f ())) ]

// ----------------------------------------------------------------------------
// Representing and interpreting expressions
// ----------------------------------------------------------------------------

let exprSend (msg: string) (args: list<string * Objekt>) (target: Objekt) =
    makeObject
        [ makeSlot "exprkind" (makeString "send")
          makeSlot "message" (makeString msg)
          makeSlot "target" target
          makeSlot
              "args"
              { empty with
                  Slots = args |> List.map (fun (name, obj) -> makeSlot name obj) }
          // Add slots:
          // - 'message' - the msg string (turn that to Self string)
          // - 'target' - storing the target expression object
          // - 'args' - object with arguments stored as slots (using List.map)
          ]

let exprConst obj =
    makeObject
        [ makeSlot "exprkind" (makeString "const")
          makeSlot "value" obj
          // Add 'value' slot storing the constant value
          ]

let exprBlock body =
    makeObject
        [ makeSlot "exprkind" (makeString "block")
          makeSlot "body" body
          // Add 'body' slot storing the expression representing the body
          ]


let rec evalExpr expr =
    let kind = expr |> send "exprkind" empty |> getStringValue

    match kind with
    | "const" ->
        // Get the value by sending 'value' to 'expr' and return it!
        expr |> send "value" empty
    | "block" ->
        // Create block using 'makeBlock'. When run, the block should
        // evaluate the 'body' of the expression recursively using 'evalExpr'
        // (to get the body, send a 'body' message to the 'expr')
        makeBlock (fun () -> evalExpr (expr |> send "body" empty))
    | "send" ->
        // Get the name of the message by sending 'message' and using 'getStringValue'
        // Recursively evaluate the 'target' using 'evalExpr'
        // Recursively evaluate values of all arguments using 'evalExpr'
        // (this is a bit long - you need to List.map over slots of 'args')
        //
        // THEN: target |> send msg args
        //
        let msg = expr |> send "message" empty |> getStringValue
        let target = expr |> send "target" empty |> evalExpr

        let args =
            { empty with
                Slots =
                    (expr |> send "args" empty).Slots
                    |> List.map (fun s ->
                        { s with
                            Contents = evalExpr s.Contents }) }

        target |> send msg args
    | _ -> failwithf "Unknown expression kind: %s" kind

// ----------------------------------------------------------------------------
// Tests - trivial hello world
// ----------------------------------------------------------------------------

let helloCode1 =
    (exprConst (makeString "Hello "))
    |> exprSend "append" [ "other", exprConst (makeString "world!") ]
    |> exprSend "print" []

// Visualise object tree to a given depth. If there are too many
// objects and the window is small, this fails. Try setting the
// limit to smaller number if this happens!
Vis.printObjectTreeLimit 3 helloCode1
helloCode1 |> evalExpr |> ignore


// ----------------------------------------------------------------------------
// Prisoner's dilemma
// ----------------------------------------------------------------------------

let betray = makeString "betray"
let coop = makeString "cooperate"

let rnd = System.Random()
let player1 = if rnd.Next(2) = 0 then betray else coop
let player2 = if rnd.Next(2) = 0 then betray else coop


// Reimplement the code to evaluate Prisoner's dilemma rules
// using the new representation. This should use only 'exprConst'
// 'exprSend' and 'exprBlock'!
let code =
    exprConst player1
    |> exprSend "equals" [ "other", exprConst coop ]
    |> exprSend
        "if"
        [ "then",
          exprBlock (
              exprConst player2
              |> exprSend "equals" [ "other", exprConst coop ]
              |> exprSend
                  "if"
                  [ "then", exprBlock (exprConst (makeString "cooperate-cooperate: each serves 1 year"))
                    "else", exprBlock (exprConst (makeString "cooperate-betray: #1 gets 3 years, #2 is free")) ]
          )
          "else",
          exprBlock (
              exprConst player2
              |> exprSend "equals" [ "other", exprConst coop ]
              |> exprSend
                  "if"
                  [ "then", exprBlock (exprConst (makeString "betray-cooperate: #1 is free, #2 gets 3 years"))
                    "else", exprBlock (exprConst (makeString "betray-betray: each serves 2 years")) ]
          ) ]
    |> exprSend "print" []

Vis.printObjectTreeLimit 3 code
evalExpr code
