// ----------------------------------------------------------------------------
// 02 - Implementing (basic) message sending
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

// Native method has a special object (F# function) as code
let makeNativeMethod f =
    makeCodeObject [] (makeSpecialObject (Native(f)))

// ----------------------------------------------------------------------------
// Lookup and message sending
// ----------------------------------------------------------------------------

// NOTE: Implemented in step #1
let rec lookup (msg: string) (obj: Objekt) : list<Slot> =
    match obj.Slots |> List.tryFind (fun s -> s.Name = msg) with
    | Some slot -> [ slot ]
    | None ->
        obj.Slots
        |> List.filter (fun s -> s.IsParent)
        |> List.map (fun s -> s.Contents)
        |> List.collect (fun o -> lookup msg o)


// See also §3.3.7 (https://handbook.selflanguage.org/SelfHandbook2017.1.pdf)
// Note that we do not need special "primitive sends". Instead, we have special
// objects and so we need to run the "native" method when we it is called.
//
// Also not that we do not yet support passing arguments to methods!

let callMethod (method: Objekt) (instance: Objekt) : Objekt =
    let arcd =
        { method with
            Slots = method.Slots @ [ makeParentSlot "receiver*" instance ] }

    match method with
    | { Special = Some(Native f) } -> f arcd
    | _ -> failwith "not implemented"

let eval (slotValue: Objekt) (instance: Objekt) : Objekt =
    // Implement the evaluation logic:
    // * If the 'slotValue' is a data object (has no 'Code') it is returned
    // * If the 'slotValue' has 'Code', we should invoke it. For now, we only
    //   handle the case where 'Code' is 'Special' and has 'Native' method.
    // * If the 'slotValue' has 'Code' that's not 'Special' fail (for now)
    //
    // To run the method we need to clone the method object (you can use the
    // F# '{ obj with ... }' syntax) and add an extra parent slot called
    // 'receiver*' that points to the 'instance' on which we invoke the method.
    //
    // NOTE: Why do we set the receiver as parent of the activation record?
    // We can then send messages to it directly to access the receiver's slots!
    match slotValue with
    | { Code = None } -> slotValue
    | { Code = Some method } -> callMethod method instance


let send (msg: string) (instance: Objekt) : Objekt =
    // Use 'lookup' to find slots with the name of the message 'msg'. If
    // there is exactly one, evaluate it using 'eval', otherwise report an error.
    match lookup msg instance with
    | [ s ] -> eval s.Contents instance
    | [] -> failwithf "No slot named %s" msg
    | _ -> failwithf "Multiple slots named %s" msg


// ----------------------------------------------------------------------------
// Helpers for testing & object construction
// ----------------------------------------------------------------------------

// Now we can reimplement 'getStringValue' using ordinary 'send'
// that follows the standard Self semantics (rather than directly)
let getStringValue (obj: Objekt) : string =
    let sObj = send "value" obj

    match sObj.Special with
    | Some(String s) -> s
    | _ -> failwith "Object is not a string"


// Define empty object with no data in it (needed below)
let empty: Objekt = makeObject []

let printCode =
    makeNativeMethod (fun arcd ->
        // Print the string value! To get the string, you can send 'value'
        // to the activation record (because this has the receiver string as a
        // parent). The returned object will be 'Special' with 'String' in it.
        // The function needs to return 'Objekt' - you can return 'empty'.
        //
        // As the first step, see what you actually pass to the method by
        // visualizing the activation record (arcd) using 'Vis.printObjectTree'!
        printfn "%s" (getStringValue arcd)
        empty)


let stringPrototype = makeObject [ makeSlot "print" printCode ]

let makeString s =
    makeObject
        [ makeSlot "value" (makeSpecialObject (String s))
          // Make 'stringPrototype' a parent of this string
          // object so that we can send the 'print' message to it!
          makeParentSlot "stringPrototype*" stringPrototype ]

// ----------------------------------------------------------------------------
// Tests - experimenting with strings
// ----------------------------------------------------------------------------

// DEMO: Create and visualize simple string object

let hello = makeString "Hello world"
hello |> send "print"

// DEMO: Create and visualize object with multiple string-object slots

let multilang =
    makeObject
        [ makeSlot "english" (makeString "Hello world")
          makeSlot "czech" (makeString "Ahoj svete")
          makeSlot "german" (makeString "Hallo Welt")
          makeSlot "french" (makeString "Bonjour monde") ]

Vis.printObjectTree multilang

multilang |> send "english" |> send "print"
multilang |> send "czech" |> send "print"


// ----------------------------------------------------------------------------
// Tests - lookups in a hierarchy of cats!
// ----------------------------------------------------------------------------

// NOTE: Now we can do all of the below just by sending messages!
// We send message to get a slot value and then send another
// message to invoke the printing method.

let cat = makeObject [ makeSlot "sound" (makeString "Meow") ]

let larry =
    makeObject [ makeParentSlot "parent*" cat; makeSlot "name" (makeString "Larry") ]
// Larry has name & sound, but no book!
larry |> send "name" |> send "print"
larry |> send "sound" |> send "print"
larry |> send "book" |> send "print"

let wonderland = makeObject [ makeSlot "book" (makeString "Alice in Wonderland") ]

let cheshire =
    makeObject
        [ makeParentSlot "cat*" cat
          makeParentSlot "fictional*" wonderland
          makeSlot "name" (makeString "Cheshire Cat") ]

// All of these should be OK!
cheshire |> send "name" |> send "print"
cheshire |> send "sound" |> send "print"
cheshire |> send "book" |> send "print"
