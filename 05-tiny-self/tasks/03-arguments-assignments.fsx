// ----------------------------------------------------------------------------
// 03 - Supporting method arguments and assignment slots
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

// To implement assignment, we need to know what object a slot
// comes from. Modify 'lookup' so that it returns not just the slot,
// but also the object that the slot comes from.
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

//  Modify 'send' and 'eval' to also take message send arguments.
// In Self, the arguments are copied into the activation record.
// In TinySelf, we use simpler trick - just make the 'args' object
// another parent of the activation record! Lookup for argument name
// in the activation record will then give us the value.
// NOTE: The object newly returned from 'lookup' should be ignored.
// BEWARE: All arguments are 'Objekt' so it is easy to swap them!!
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


// Implement method to append one string to another.
// Start by visualizing the activation record using 'Vis.printObjectTree'!
//
// The first string is the current instance. The activation record
// has the current instance as a parent, so you can access its slots
// directly through the activation record (using 'getStringValue').
// The second string is an argument named 'other', so you need to get
// that using 'send'
//
// NOTE: This is now recursively defined so that you can create a new
// string value inside 'appendCode' using 'makeString'.
let rec appendCode =
    makeNativeMethod (fun arcd ->
        let first = arcd |> getStringValue
        let second = arcd |> send "other" empty |> getStringValue

        makeString (first + second))

and stringPrototype =
    makeObject [ makeSlot "print" printCode; makeSlot "append" appendCode ]

and makeString s =
    makeObject
        [ makeSlot "value" (makeSpecialObject (String s))
          makeParentSlot "string*" stringPrototype ]

// ----------------------------------------------------------------------------
// Tests - printing
// ----------------------------------------------------------------------------

// Append a bunch of strings and print the result!

let s1 = makeString "hello"
let s2 = makeString " "
let s3 = makeString "world"
let s4 = makeString "!"

s1
|> send "append" (makeObject [ makeSlot "other" s2 ])
|> send "append" (makeObject [ makeSlot "other" s3 ])
|> send "append" (makeObject [ makeSlot "other" s4 ])
|> send "print" empty
|> ignore

// ----------------------------------------------------------------------------
// Assignment slots
// ----------------------------------------------------------------------------

let assignmentMethod n =
    makeNativeMethod (fun arcd ->
        // The activation record has a slot named 'n' (name given as the
        // argument) somewhere in its inheritance graph and a slot named 'new'
        // ('new' being the actual slot name) which is a method argument.
        // Find those two using 'lookup' and modify the slot value (in the
        // that contained it - as returned from lookup). (Tiny)Self assignment
        // should return the object that has been modified.
        // NOTE: Mutate the slots using 'obj.Slots <- newSlots'!
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

// Creates an assignment slot for a slot named 'n'
let makeAssignmentSlot n =
    { Name = n + ":"
      Contents = assignmentMethod n
      IsParent = false }


// ----------------------------------------------------------------------------
// Tests - over-engineered prototype-based Hello world
// ----------------------------------------------------------------------------

let greeter =
    makeObject
        [ makeSlot "greeting" (makeString "Hello")
          makeSlot
              "greet"
              (makeNativeMethod (fun acrd ->
                  // This is implemented as native method, but note that it is using
                  // only TinySelf operations - it is creating primitive string values
                  // and sending messages. We do not need any F# here!
                  let who = send "who" empty acrd
                  let sp, ex = makeString " ", makeString "!"

                  send "greeting" empty acrd
                  |> send "append" (makeObject [ makeSlot "other" sp ])
                  |> send "append" (makeObject [ makeSlot "other" who ])
                  |> send "append" (makeObject [ makeSlot "other" ex ])
                  |> send "print" empty)) ]

// Hello world has an assignment slot for 'who'
let helloWorld =
    makeObject
        [ makeParentSlot "greeter*" greeter
          makeSlot "who" (makeString "world")
          makeAssignmentSlot "who" ]
// .. but Hello Matfyz doesn't, so we cannot change its 'who'.
let helloMatfyz =
    makeObject [ makeParentSlot "greeter*" greeter; makeSlot "who" (makeString "Matfyz") ]

helloWorld |> send "greet" empty |> ignore
helloMatfyz |> send "greet" empty |> ignore

// This changes the 'who'. Run the above snippet to verify this!
helloWorld |> send "who:" (makeObject [ makeSlot "new" (makeString "svete") ])
// This throws an exception - there is no assignment slot.
helloMatfyz |> send "who:" (makeObject [ makeSlot "new" (makeString "CVUT") ])

// We can create assignment slots in derived objects and
// the assignemtn changes the value in the parent!
let greetingSetter =
    makeObject [ makeParentSlot "greeter*" greeter; makeAssignmentSlot "greeting" ]
// We can invoke the assignment slot in 'greetingSetter'
greetingSetter
|> send "greeting:" (makeObject [ makeSlot "new" (makeString "Ahoj") ])
// But not in another object that has 'greeter' as parent
helloMatfyz
|> send "greeting:" (makeObject [ makeSlot "new" (makeString "Ahoj") ])

Vis.printObjectsTree [ helloMatfyz; helloWorld; greetingSetter ]
