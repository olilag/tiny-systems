// ----------------------------------------------------------------------------
// 06 - Adding 'new', 'self' and 'method' expressions
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

let makeBoolean b : Objekt =
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
                  Slots = args |> List.map (fun (name, obj) -> makeSlot name obj) } ]

let exprConst (obj: Objekt) : Objekt =
    makeObject [ makeSlot "exprkind" (makeString "const"); makeSlot "value" obj ]

let exprBlock (body: Objekt) : Objekt =
    makeObject [ makeSlot "exprkind" (makeString "block"); makeSlot "body" body ]

let exprString (s: string) : Objekt =
    // Implement helper for string literal expressions. This is just
    // creating a constant (exprConst) from the given string value.
    exprConst (makeString s)

let exprSelf: Objekt =
    // Add new type of expression! This represents access to the
    // object itself (self / this). See notes below in 'evalExpr'.
    makeObject [ makeSlot "exprkind" (makeString "self") ]

let exprMethod (body: Objekt) : Objekt =
    // Add new type of expression! This represents a
    // method object. See notes below in 'evalExpr'.
    makeObject [ makeSlot "exprkind" (makeString "method"); makeSlot "body" body ]

let exprNew (slots: list<string * Objekt>) : Objekt =
    // Add a new type of expression! This represents creation of an
    // object with a given list of slots and expressions for the slots.
    //
    // NOTE #1: You will need to store the expressions of slots somehow.
    // This can be done by creating an object that stores them as slots
    // (or by adding them to the current object as slots after 'exprkind')
    //
    // NOTE #2: Later, we will want to recognize 'name*' and 'name:' syntax
    // for creating parent slots and assignment slots. But now, we just need
    // to store the name (so that we can create parent/assignment slot in
    // 'evalExpr'). As 'makeSlot' checks that the name does *not* end with '*',
    // you need some kind of escaping. E.g. do '.Replace("*","_ASTERISK_")'
    // here when storing the expression and then reverse the replace in 'evalExpr'.
    makeObject (
        [ makeSlot "exprkind" (makeString "new") ]
        @ (slots
           |> List.map (fun (name, obj) -> makeSlot (name.Replace("*", "_ASTERISK_")) obj))
    )


let rec evalExpr (arcd: Objekt) (expr: Objekt) : Objekt =
    // We added 'arcd' (activation record) argument to the 'evalExpr'
    // function, because we need this when evaluating the 'self' expression!
    //
    // You need to handle three new kinds of expressions:
    //
    // * 'self' - This should return the receiver from the activation record
    //   (review the 'eval' implementation where the activation record is created)
    //
    // * 'new' - This is a bit like the handling of 'send'. You need to recursively
    //   evaluate all the expressions that we want to assign as slots to the
    //   object. Then, based on the name, create the right kind of slot!
    //   We want to support 'name*' for parent slots and 'name:'. For this,
    //   you need to create both normal slot (name) and assignment slot (name:)
    //
    // * 'method' - This is very similar to the 'block' case, but you need to
    //   return method using 'makeNativeMethod'. The method takes a different
    //   activation record than the one passed to 'evalExpr'! This way it can
    //   access the object in which it exists (not the acrd of the code that
    //   defined it...)
    let kind = expr |> send "exprkind" empty |> getStringValue

    match kind with
    | "const" -> expr |> send "value" empty
    | "block" -> makeBlock (fun () -> evalExpr arcd (expr |> send "body" empty))
    | "send" ->
        let msg = expr |> send "message" empty |> getStringValue
        let target = expr |> send "target" empty |> evalExpr arcd

        let args =
            { empty with
                Slots =
                    (expr |> send "args" empty).Slots
                    |> List.map (fun s ->
                        { s with

                            Contents = evalExpr arcd s.Contents }) }

        target |> send msg args
    | "self" -> arcd |> send "receiver*" empty
    | "new" ->
        let slots = expr.Slots |> List.filter (fun s -> not (s.Name = "exprkind"))

        { empty with
            Slots =
                slots
                |> List.collect (fun s ->
                    if s.Name.Contains ":" then
                        let name = s.Name.Substring(0, s.Name.Length - 1)
                        [ makeSlot name (evalExpr arcd s.Contents); makeAssignmentSlot name ]
                    else
                        [ { s with
                              Name = s.Name.Replace("_ASTERISK_", "*")
                              IsParent = s.Name.Contains "_ASTERISK_"
                              Contents = evalExpr arcd s.Contents } ]) }
    | "method" ->
        let body = expr |> send "body" empty

        makeNativeMethod (fun arcd -> evalExpr arcd body)
    | _ -> failwithf "Unknown expression kind: %s" kind

let makeSelfMethod expr =
    // NOTE: Here we now call 'evalExpr' with 'arcd' as argument!
    makeNativeMethod (fun arcd -> evalExpr arcd expr)


// ----------------------------------------------------------------------------
// Tests - trivial hello world (same as before)
// ----------------------------------------------------------------------------

let helloCode1 =
    exprString "Hello "
    |> exprSend "append" [ "other", exprString "world!" ]
    |> exprSend "print" []

Vis.printObjectTreeLimit 3 helloCode1
helloCode1 |> evalExpr empty |> ignore


let helloCode2 =
    exprString "hello"
    |> exprSend "append" [ "other", exprString " " ]
    |> exprSend "append" [ "other", exprString "world" ]
    |> exprSend "append" [ "other", exprString "!" ]
    |> exprSend "print" []

//Vis.printObjectTreeLimit 3 helloCode2
helloCode2 |> evalExpr empty |> ignore


// ----------------------------------------------------------------------------
// Tests - over-engineered prototype-based Hello world
// ----------------------------------------------------------------------------

// This is the same demo as in Step 3, but now the entire demo is written
// as Self expressions that we later evaluate using 'evalExpr'. In other
// words, all the code below just constructs Self program that, when run,
// creates the appropriate Self objects!

let greeter =
    exprNew
        [ "greeting", exprString "Hello"
          "greet",
          exprMethod (
              let who = exprSend "who" [] exprSelf
              let sp, ex = exprString " ", exprString "!"

              exprSend "greeting" [] exprSelf
              |> exprSend "append" [ "other", sp ]
              |> exprSend "append" [ "other", who ]
              |> exprSend "append" [ "other", ex ]
              |> exprSend "print" []
          ) ]

let helloWorld = exprNew [ "greeter*", greeter; "who:", exprString "world" ]

let helloMatfyz = exprNew [ "greeter*", greeter; "who", exprString "Matfyz" ]

// Create the Self objects and send them the right messages
// Use 'Vis' to make sure that you get the correct objects!

let greeterObj = greeter |> evalExpr empty
let helloWorldObj = helloWorld |> evalExpr empty
let helloMatfyzObj = helloMatfyz |> evalExpr empty

Vis.printObjectTreeLimit 2 helloWorldObj
Vis.printObjectTreeLimit 2 helloMatfyzObj
Vis.printObjectTreeLimit 2 greeterObj

helloWorldObj |> send "greet" empty |> ignore
helloMatfyzObj |> send "greet" empty |> ignore

// This works because 'helloWorld' has an assignment slot
helloWorldObj
|> send "who:" (makeObject [ makeSlot "new" (makeString "svete") ])
// This does not work because 'helloMatfyz' has only regular slot
helloMatfyzObj
|> send "who:" (makeObject [ makeSlot "new" (makeString "CVUT") ])
