#r "D:/Data/Dev/Archival-Mess/FSharp.Data/src/FSharp.Data/bin/Debug/netstandard2.0/FSharp.Data.Http.dll"
#r "D:/Data/Dev/Archival-Mess/FSharp.Data/src/FSharp.Data/bin/Debug/netstandard2.0/FSharp.Data.Runtime.Utilities.dll"
#r "D:/Data/Dev/Archival-Mess/FSharp.Data/src/FSharp.Data/bin/Debug/netstandard2.0/FSharp.Data.dll"
#r "D:/Data/Dev/Archival-Mess/FSharp.Data/src/FSharp.Data/bin/Debug/netstandard2.0/FSharp.Data.DesignTime.dll"
#r "D:/Data/Dev/Archival-Mess/FSharp.Data/src/FSharp.Data/bin/Debug/netstandard2.0/FSharp.Data.Xml.Core.dll"
#r "D:/Data/Dev/Archival-Mess/FSharp.Data/src/FSharp.Data/bin/Debug/netstandard2.0/FSharp.Data.Csv.Core.dll"
#r "D:/Data/Dev/Archival-Mess/FSharp.Data/src/FSharp.Data/bin/Debug/netstandard2.0/FSharp.Data.Json.Core.dll"
#r "D:/Data/Dev/Archival-Mess/FSharp.Data/src/FSharp.Data/bin/Debug/netstandard2.0/FSharp.Data.Json.Core2.dll"

// #r "nuget: FSharp.Data.Xml.Core, 6.0.1-beta002"
// #r "nuget: FSharp.Data.Json.Core, 6.0.1-beta002"
// #r "nuget: FSharp.Data.Csv.Core, 6.0.1-beta002"
// #r "nuget: FSharp.Data.Http, 6.0.1-beta002"
// #r "nuget: FSharp.Data, 6.0.1-beta002"
// #r "nuget: FSharp.Data.Html.Core, 6.0.1-beta002"

type ChildlessInferredType =
    | Null
    | Primitive of isOptional: bool
type ParentInferredType =
    | Collection of isOptional: bool
    | Record of isOptional: bool
    | Heterogeneous
type HandledInferredType =
    | Childless of ChildlessInferredType
    | Parent of ParentInferredType
let allCases =
    [
        Childless(Null)
        Childless(Primitive(true))
        Childless(Primitive(false))
        Parent(Collection(true))
        Parent(Collection(false))
        Parent(Record(true))
        Parent(Record(false))
        Parent(Heterogeneous) 
    ]
let allParents =
    [
        Collection(true)
        Collection(false)
        Record(true)
        Record(false)
        Heterogeneous
    ]

let allSamples =
    [
        for parent in allParents do
            for child in allCases do
                match parent, child with
                | Collection(true), Childless(Null) -> """[ null, [null] ]"""
                | Collection(false), Childless(Null) -> """[ [null] ]"""
                | Collection(true), Childless(Primitive(true)) -> """[ null, [null], [1] ]"""
                | Collection(true), Childless(Primitive(false)) -> """[ null, [1] ]"""
                | Collection(false), Childless(Primitive(true)) -> """[ [null], [1] ]"""
                | Collection(false), Childless(Primitive(false)) -> """[ [1] ]"""
                | Collection(true), Parent(Collection(true)) -> """[ null, [null], [[1]] ]""" // child collection without any type specified is a bit useless. try with 1, "a", true, other stuff too?
                | Collection(true), Parent(Collection(false)) -> """"""
                | Collection(false), Parent(Collection(true)) -> """"""
                | Collection(false), Parent(Collection(false)) -> """"""
                | Collection(true), Parent(Record(true)) -> """[ null, [null], [{"a": 1}] ]""" // same here: add a few properties of different types? or not. it's the record we want to test...
                | Collection(true), Parent(Record(false)) -> """[ null, [{"a": 1}] ]"""
                | Collection(false), Parent(Record(true)) -> """"""
                | Collection(false), Parent(Record(false)) -> """"""
                | Collection(true), Parent(Heterogeneous) -> """[ null, [1], ["a"], [{"a":1}] ]""" // how to test child heterogeneous types? 1 and "a" only? add a record too? an array?...
                | Collection(false), Parent(Heterogeneous) -> """[ [1], ["a"], [{"a":1}] ]"""

                | Record(true), Childless(Null) -> """[ null, {"a":null} ]"""
                | Record(false), Childless(Null) -> """[ {"a":null} ]"""
                | Record(true), Childless(Primitive(true)) -> """"""
                | Record(true), Childless(Primitive(false)) -> """"""
                | Record(false), Childless(Primitive(true)) -> """"""
                | Record(false), Childless(Primitive(false)) -> """"""
                | Record(true), Parent(Collection(true)) -> """"""
                | Record(true), Parent(Collection(false)) -> """"""
                | Record(false), Parent(Collection(true)) -> """"""
                | Record(false), Parent(Collection(false)) -> """"""
                | Record(true), Parent(Record(true)) -> """"""
                | Record(true), Parent(Record(false)) -> """"""
                | Record(false), Parent(Record(true)) -> """"""
                | Record(false), Parent(Record(false)) -> """"""
                | Record(true), Parent(Heterogeneous) -> """"""
                | Record(false), Parent(Heterogeneous) -> """"""

                | Heterogeneous, Childless(Null) -> """[ 1, "a", null ]""" // useless case: null is always an allowed value for a heterogeneous type. (is that what we want? do we have a choice? it's a bit unavoidable since the backing type is an option and we convert null tokens to None...)
                | Heterogeneous, Childless(Primitive(true)) -> """[ null, 1, "a" ]""" // same here? same for all other types? null is always a possibe value?
                | Heterogeneous, Childless(Primitive(false)) -> """"""
                | Heterogeneous, Parent(Collection(true)) -> """"""
                | Heterogeneous, Parent(Collection(false)) -> """"""
                | Heterogeneous, Parent(Record(true)) -> """"""
                | Heterogeneous, Parent(Record(false)) -> """"""
                | Heterogeneous, Parent(Heterogeneous) -> """"""
    ]


open FSharp.Data
type PT = JsonProvider2<""" [ 1, "a" ] """, SampleIsList=true>
let xx = PT.Parse(""" [] """)
let a = xx.Value.JsonNode

// """ {"a": [ {"b": 1}, null ]}  """ // optional record
// """ {"a": [ [1], null ]}  """ // optional collection
// """ {"a": [ 1, null ]}  """ // optional primitive types

// type PT = JsonProvider2<""" { "aa": null, "a": [ 1, "x", null, [42], {"b": 123} ]} """> // heterogeneous types
// let xx = PT.Parse(""" { "aa": 1, "a": [ 1, "x", null, [42], {"b": 123} ]} """)
// let aa = xx.Aa |> Option.map (fun x -> x.JsonNode.GetValue<int>()) |> Option.defaultValue 0
// let yy = xx.A
// yy[3]
// yy[0].Number.Value
// yy[1].String.Value
// yy[2].IsNone
// yy[3].Array
// let zz = yy[4].Record.Value
// zz.B

#r "nuget: System.Text.Json"
open System
open System.Text.Json
open System.Text.Json.Nodes
open System.Text.Json.Serialization
open System.Text.Json.Nodes
open FSharp.Data
open FSharp.Data.Runtime.BaseTypes
open FSharp.Data.Runtime

// let test = System.Text.Json.JsonDocument.Parse(""" {"a":"1"} """, JsonDocumentOptions(CommentHandling=JsonCommentHandling.Skip, AllowTrailingCommas=true))
// let jsonVal = test.RootElement.GetProperty("a")
// let serializerOptions = JsonSerializerOptions(NumberHandling=JsonNumberHandling.AllowReadingFromString)
// System.Text.Json.JsonSerializer.Deserialize<float>(jsonVal, serializerOptions)
// jsonVal.Deserialize<float>(serializerOptions)

// let testNode2 = System.Text.Json.Nodes.JsonObject.Create(test.RootElement)
// testNode2.Parent

// let testNode = System.Text.Json.Nodes.JsonNode.Parse(""" {"a":null} """,documentOptions=JsonDocumentOptions(CommentHandling=JsonCommentHandling.Skip, AllowTrailingCommas=true))
// testNode["b"] <- JsonValue.Create(null)
// testNode.ToJsonString()
// let x,y = testNode.AsObject().TryGetPropertyValue("ab")
// y.GetValue<double>()

[<Struct>]
type JsonWrapper(node: JsonNode) =
    //new(json: string) = JsonWrapper(JsonNode.Parse(json))
    member this.JsonNode = node

// let getValueKind (node: JsonNode) =
//     match node with
//     | null -> JsonValueKind.Null
//     | :? JsonObject -> JsonValueKind.Object
//     | :? JsonArray -> JsonValueKind.Array
//     | :? Nodes.JsonValue as jValue ->
//         match jValue.TryGetValue<JsonElement>() with
//         | true, element -> element.ValueKind
//         | false, _ -> JsonValueKind.Undefined // TODO (can only happen when json values have been manually created (not from parsing))
//     | shouldNotBePossible -> failwith $"node implementation was of an unexpected type ({shouldNotBePossible.GetType()})."
    
let parseJson jsonString =
    JsonNode.Parse(
        (jsonString : string),
        documentOptions = JsonDocumentOptions(CommentHandling = JsonCommentHandling.Skip, AllowTrailingCommas = true))
    |> Option.ofObj

"""
{"a": [ 1, null ]}
//{ "aa": null, "a": [ 1, "x", null, [42], {"b": 123} ]} 
""" |> parseJson

type JsonRuntime2 =
    static member private FormatJsonForException(json: string) =
        if isNull json then "`null`"
        else
            let truncatedJson = json.Substring(0, Math.Min(100, json.Length))
            let truncationMark = if truncatedJson.Length = json.Length then "" else " [...]"
            $"`{truncatedJson}{truncationMark}`"

    static member GetPrimitiveValue(json: JsonWrapper) =
        match json.JsonNode with
        | null -> failwith $"Trying to get primitive value: expected node to be a {nameof Nodes.JsonValue} but was `null`."
        | :? Nodes.JsonValue as value ->
            let mutable result = Unchecked.defaultof<_>
            if value.TryGetValue<'TValue>(&result) then
                result
            else
                try
                    // Deserialize() is required when the node was manually created and is not backed by a JsonElement but by a concrete type.
                    // Unfortunately if this fails the exception doesn't contain the full json path, so we have to catch it and rethrow to have it...
                    value.Deserialize<'TValue>()
                with | ex ->
                    raise (Exception($"Could not convert JsonValue into a {typeof<'TValue>} at path {value.GetPath()}. Node value: {JsonRuntime2.FormatJsonForException(value.ToJsonString())}", ex))
        | other -> failwith $"Trying to get primitive value at {json.JsonNode.GetPath()}: expected node to be a {nameof Nodes.JsonValue} but was {other.GetType()}. Node value: {JsonRuntime2.FormatJsonForException(other.ToJsonString())}"

    static member GetRecordProperty(json: JsonWrapper, propertyName) =
        match json.JsonNode with
        | null -> failwith $"Trying to get property \"{propertyName}\": expected node to be a {nameof JsonObject} but was `null`."
        | :? JsonObject as obj ->
            let mutable result = Unchecked.defaultof<_>
            if obj.TryGetPropertyValue(propertyName, &result) then
                JsonWrapper(result)
            else
                failwith $"Property {propertyName} does not exist at {json.JsonNode.GetPath()}. Node value: {JsonRuntime2.FormatJsonForException(json.JsonNode.ToJsonString())}"
        | other -> failwith $"Trying to get property \"{propertyName}\": expected node at {json.JsonNode.GetPath()} to be a {nameof JsonObject} but was {other.GetType()}. Node value: {JsonRuntime2.FormatJsonForException(other.ToJsonString())}"

    static member GetArrayValues(json: JsonWrapper, convertItem: Func<JsonWrapper, 'TValue>) =
        match json.JsonNode with
        | null -> failwith $"Trying to get array: expected node to be a {nameof JsonArray} but was `null`."
        | :? JsonArray as array ->
            [|
                for item in array ->
                    try
                        JsonWrapper(item) |> convertItem.Invoke
                    with ex ->
                        if isNull item then raise (Exception(failwith $"Could not convert JsonArray into values of type {typeof<'TValue>} at path {array.GetPath()} because one of the items was `null`. Array node value: {JsonRuntime2.FormatJsonForException(array.ToJsonString())}", ex))
                        else raise (Exception($"Could not convert JsonArray into values of type {typeof<'TValue>} at path {item.GetPath()}. Item node value: {JsonRuntime2.FormatJsonForException(item.ToJsonString())}", ex))
            |]
        | other -> failwith $"Trying to get array: expected node at {json.JsonNode.GetPath()} to be a {nameof JsonArray} but was {other.GetType()}. Node value: {JsonRuntime2.FormatJsonForException(other.ToJsonString())}"

let node = JsonWrapper(JsonNode.Parse("""
{
    "a": {"b":42}
}"""))

let a = JsonRuntime2.GetRecordProperty(node,"a")
let b = JsonRuntime2.GetRecordProperty(a,"b")
JsonRuntime2.GetPrimitiveValue<bool>(b)
b.JsonNode.AsValue().TryGetValue<bool>()
b.JsonNode.AsValue().Deserialize<bool>()
b.JsonNode.AsValue().GetPath()
// let getRecordProperty propertyName (node: JsonNode) =
//     match node with
//     | null -> failwith $"Trying to get property '{propertyName}': expected node to be a {nameof JsonObject} but was null."
//     | :? JsonObject as obj ->
//         match obj.TryGetPropertyValue(propertyName) with
//         | true, prop -> prop
//         | false, _ -> failwith $"Property {propertyName} does not exist at {node.GetPath()}."
//     | other -> failwith $"Trying to get property '{propertyName}': expected node at {node.GetPath()} to be a {nameof JsonObject} but was {other.GetType()}."

// let getArrayValue (index: int) (node: JsonNode) =
//     match node with
//     | null -> failwith $"Trying to get array value at index '{index}': expected node to be a {nameof JsonArray} but was null."
//     | :? JsonArray as array ->
//         try array[index]
//         with | :? ArgumentOutOfRangeException -> failwith $"Trying to get array value at index '{index}': index is out of range for node at {node.GetPath()}."
//     | other -> failwith $"Trying to get array value at index '{index}': expected node at {node.GetPath()} to be a {nameof JsonArray} but was {other.GetType()}."

// let getPrimitiveValue<'TValue> (node: JsonNode) =
//     match node with
//     | null -> failwith $"Trying to get primitive value: expected node to be a {nameof Nodes.JsonValue} but was null."
//     | :? Nodes.JsonValue as value ->
//         match value.TryGetValue<'TValue>() with
//         | true, value -> value
//         | false, _ -> value.Deserialize<'TValue>() // Required when the node was manually created and is not backed by a JsonElement but by a concrete type.
//     | other -> failwith $"Trying to get primitive value at {node.GetPath()}: expected node to be a {nameof Nodes.JsonValue} but was {other.GetType()}."

let testJson = parseJson """ {"null":null, "int":1, "bool":true, "string":"asdf", "array": [null, 1, true, "asdf", [], {}], "obj": {"null":null, "int":2, "bool":false, "string":"asdf2", "array":[2], "obj": {}} } """
let testJson = testJson.Value

let root =
    let x = parseJson """{ "a": { "a1": 1 } }"""
    x.Value

// provided type:
type Root(json) =
    member this.A with get () = json |> getRecordProperty "a" |> A
and A(json) =
    member this.A1 with get () = json |> getRecordProperty "a1" |> getPrimitiveValue<int>

let test = Root(root)
test.A.A1

let testJson =
    let root = JsonObject()
    root["null"] <- null
    root["int"] <- 1
    root["bool"] <- true
    root["string"] <- "asdf"
    root["array"] <- JsonArray(null, 1, true, "asdf", JsonArray(), JsonObject())
    let obj = JsonObject()
    obj["null"] <- null
    obj["int"] <- 2
    obj["bool"] <- false
    obj["string"] <- "asdf2"
    obj["array"] <- JsonArray(2)
    obj["obj"] <- JsonObject()
    root["obj"] <- obj
    root :> JsonNode

let serializerOptions = JsonSerializerOptions(NumberHandling=JsonNumberHandling.AllowReadingFromString)
let test2 = System.Text.Json.JsonSerializer.Deserialize<JsonObject>("""{"a":"1"}""", serializerOptions)
test2["a"] .AsValue().Deserialize<int>() // doesnt work

getRecordProperty "a" testJson["null"]
getRecordProperty "a" (testJson["array"])
getArrayValue 0 (testJson["null"])
getArrayValue 0 (testJson["array"])
getPrimitiveValue<int> (testJson["null"])
getPrimitiveValue<int> (testJson["array"])
getPrimitiveValue<int> (testJson["int"])
getPrimitiveValue<int64> (testJson["int"])
getPrimitiveValue<double> (testJson["int"])
getPrimitiveValue<decimal> (testJson["int"])
getPrimitiveValue<bool> (testJson["int"])
getPrimitiveValue<string> (testJson["int"])

getPrimitiveValue<string> (testJson["array"][0])
getPrimitiveValue<string> (testJson["array"][1])
getPrimitiveValue<string> (testJson["array"][3])

let serializeJson (node: JsonNode) =
    // TODO: cache/reuse serializer options https://github.com/dotnet/runtime/issues/38982#issuecomment-656491611
    let serializerOptions = JsonSerializerOptions(NumberHandling=JsonNumberHandling.AllowReadingFromString)
    System.Text.Json.JsonSerializer.Serialize(node, serializerOptions)

serializeJson (JsonValue.Create(null))

let ``get primitive from record`` () =
    let node = parseJson """ {"a":1} """
    let result = node |> Option.map (getRecordProperty "a")
    result

// #r "nuget: Newtonsoft.Json"
// let jsonnetNull = Newtonsoft.Json.Linq.JToken.Parse("null")
// jsonnetNull.ToString(Newtonsoft.Json.Formatting.None)
// //
// #r "nuget: System.Text.Json"
// let stjNull = System.Text.Json.Nodes.JsonNode.Parse("null")
// stjNull.ToJsonString()
// //
// let asf = System.Text.Json.Nodes.JsonNode.Parse("[1,\"asdf\"]")
// //
// let asf = System.Text.Json.Nodes.JsonValue.Parse("nulld")// .Create((1,2))
// //
// System.Text.Json.JsonSerializer.Serialize(System.Text.Json.JsonSerializer.Deserialize<System.Text.Json.Nodes.JsonNode>("""null"""))
// //
// System.Text.Json.Nodes.JsonValue.Create(42).GetValue<double>()
// let x =System.Text.Json.Nodes.JsonValue.Parse(""" "1" """)
// let jdoc = System.Text.Json.JsonDocument.Parse(""" 1 """)
// jdoc.RootElement.ValueKind
// let asdf = System.Text.Json.Nodes.JsonNode.op_Explicit(x) : int

// open System.Xml.Linq
// XElement("a",42) = XElement("a",42)

open FSharp.Data
// open ProviderImplementation

// type InlineSchemas = JsonProvider<""" {"a": "typeof<int<metre>>"} """, InferenceMode=InferenceMode.ValuesAndInlineSchemasOverrides>
// let serialized = InlineSchemas.Root(42).JsonValue.ToString(JsonSaveOptions.DisableFormatting)
// serialized |> should equal """{"a":42}"""

// todo: unit test preferdictionaries avec {"123": null, "456":42} -> impossible de créer un item avec une value null
// ça serait mieux si preferdictionaries existait pas et qu'on avait toujours un ctor en mode dictionary pour les records non ? (et une property pour lire les items as dictionary ?)
//  ou alors on garde preferdictionaries mais il faut un type hint qqpart

// todo: je crois qu'il faut rajouter une info d'optionability sur les collections -> oui. on a toujours un OptionalSingle si l'array est manquant ou si l'array est null
// todo: et peut être aussi sur les heterogeneous? (ou au moins la prendre en compte ? tester su)
// todo: pour les heterogeneous il faut avoir un ctor qui permet de passer null meme si c'était que des int et bool par exemple (un "OrNothing" ?)

[<Literal>]
let sample =
    """[
// // {"a": 42, "b": null } // b is explicitly null
// {"a": 42 } // b is missing
// {"a": 42, "b": "test"}

// {"a": null} // a is explicitly null
// {} // a is missing
// {"a": ""}
// {"a": "asdf"}

// primitive:
// {"a": 42, "b": null} // b is explicitly null
// // {"a": 42} // b is missing
// {"a": 42, "b": 43}

// heterogeneous:
// // {"a": 42, "b": null} // b is explicitly null
// {"a": 42} // b is missing
// {"a": 42, "b": 43}
// {"a": 42, "b": "asdf"}
 
// collections:
// {"a": 42, "b": null } // b is explicitly null
// {"a": 42 } // b is missing 
// {"a": 42, "b": [ "test", "asdf"]}

// TODO (failing in ToJsonValue2 with "Some [||]") DONE
// {"a": null}, // a is explicitly null
// {}, // a is missing
// {"a": [ 1 ]}

// TODO: no way to set inner array as null (generates a ctor with int[][] instead of option<array<int>>[]) -> DONE
// {"a": [[1], [2], null]} 
// {"a": [[1], [2]]} 

// {"a": [[1], [2], "asdf", null]} 
 
// DONE: should generate an int option array
// {"a": [1,2,null]}
// {"a": [1,2]}

// {"a": [1,2,"", null]}
// {"a": [1,2,""]}

// {"a": ["a"]}
//,
// {"a": {"b": 42}}
// {"a": null}
// {}

// array with records and nulls
//  {"a": [ {"b":1}, null ] } 

// array with primitive types and nulls
// {"a": [ 1, null ] } 

// array with explicit null token
// {"a": [ null ] } 

// array with child array and nulls
// {"a": [ [1], null ] }

// array with heterogenous items and nulls
// {"a": [ 1, {"b":2}, null ] }

// heterogeneous with string inferred as int (ctor/serialization test)
// {"a": "1"},{"a": {"b":null}}, {}

// {"a": 42}
// ,{"a": {"a": null}}
// // ,{"a": []}
// ,{"a": null}
// ,{}

{"a": [ 42, [] ]}
 
]"""

type PT = JsonProvider2<sample>
// let testout = ((PT.Parse sample)[0]).A[1].String
let testout = ((PT.Parse sample)[0]).A
let asdf = testout.A.Number
// let testin1 = PT.Root(Some <| PT.NumbersOrStrings([|1|], [|"asdf"|]))
PT.Root(PT.IntOrA())
let aa = PT.IntOrA(1)
aa.JsonValue.ToString()
PT.Root([|Some <| PT.A(1)|])
PT.Root([|Some [|2|]; None|])
PT.Root([| PT.StringOrArray([|1|]); PT.StringOrArray(); PT.StringOrArray("asdf") |])
PT.Root([| PT.IntOrString(); PT.IntOrString(1) |])
PT.Root([| None |])
PT.Root([| Some 42; None |])

type PT0 = JsonProvider<sample>
let asdf = (PT0.Parse(sample))
asdf.A.JsonValue.ToString()
 

// Xml
[<Literal>]
let sample =
    """
    <root>
        <a i="1" j="1" x="10" y="10">asdf</a>
        <a i="1" j="" x="true" y="true"></a>
    </root>
    """
// bug?: impossible to generate attribute of type int that is there but empty ("")...
type PTX = XmlProvider<sample>

let asdf = (PTX.Parse(sample).As[0])
let asdf2 = asdf.J
PTX.A(1,None, PTX.XChoice(), PTX.YChoice(), None)
PTX.Root(1,[| PTX.A(PTX.XChoice(), PTX.YChoice(1), Some "asdf") |])

// // Multiplicity:
// type PT1 = JsonProvider< """{"a": [42,"asdf"]}""">
// type PT2 = JsonProvider2<"""{"a": [42,"asdf"]}""">
// let test1= PT1.Root(a=PT1.IntOrString(42, "asdf"))
// let test2= PT2.Root(a=PT2.NumbersOrStrings([|42|], [|"asdf"|]))

// // Empty string:
// type PT1 = JsonProvider< """{"a": ""} {"a": "asdf"}""", SampleIsList=true>
// type PT2 = JsonProvider2<"""{"a": ""} {"a": "asdf"}""", SampleIsList=true>
// let test1= PT1.GetSamples() |> Array.map (fun x -> x.A)
// let test2= PT2.GetSamples() |> Array.map (fun x -> x.A)

// // Bools inference:
// type PT1 = JsonProvider< """{"a": 0} {"a": 1} {"a": "yes"}""", SampleIsList=true>
// type PT2 = JsonProvider2<"""{"a": 0} {"a": 1} {"a": "yes"}""", SampleIsList=true>
// let test1= PT1.GetSamples() |> Array.map (fun x -> x.A)
// let test2= PT2.GetSamples() |> Array.map (fun x -> x.A)



// let o1 = true
// let o2 = true
// let allowEmptyValues = true
// let canHaveEmptyValues = true
// let result = (o1 || o2) && not (allowEmptyValues && canHaveEmptyValues)

// let getExpected o1 o2 allowEmptyValues canHaveEmptyValues =
//     (o1 || o2) && not (allowEmptyValues && canHaveEmptyValues)

// let getTest o1 o2 allowEmptyValues canHaveEmptyValues =
//     if allowEmptyValues && canHaveEmptyValues then false
//     else (o1 || o2)

// for o1 in [true;false] do
//     for o2 in [true;false] do
//         for allowEmptyValues in [true;false] do
//             for canHaveEmptyValues in [true;false] do
//                 let expected = getExpected o1 o2 allowEmptyValues canHaveEmptyValues
//                 let actual = getTest o1 o2 allowEmptyValues canHaveEmptyValues
//                 printfn $"ok={expected=actual}, expected={expected}, actual={actual},o1={o1}, o2={o2}, allowEmptyValues={allowEmptyValues}, canHaveEmptyValues={canHaveEmptyValues}"




// type PT = JsonProvider<"""
// // {
    // //     "Mappings": {
        // //         "123": [
            // //             {
                // //                 "GroupId": 1001,
                // //                 "CanDelete": true
                // //             },
                // //             {
                    // //                 "GroupId": 6562,
                    // //                 "CanDelete": true
                    // //             }
                    // //         ],
                    // //         "456": [
                        // //             {
// //                 "GroupId": 1001,
// //                 "CanDelete": false
// //             }
// //         ],
// //         "789": [
// //             {
// //                 "GroupId": 1001,
// //                 "CanDelete": false,
// //                 "ErrorMessage": "Error."
// //             },
// //             {
// //                 "GroupId": 6562,
// //                 "CanDelete": true
// //             }
// //         ]
// //     }
// // }

// // {
// //     "123": 10,
// //     "456": 11
// // }

// // {
// //     "123": {"a": "42"},
// //     "456": {"a": "43"}
// // }

// //wtf multiplicity fuck you:
// // {
// //     "a": [ "1", true, false, "1", 1 ]
// // }

// // crash if try to access string (multiplicity bug sur la string)
// // {
// //     "a": [ "1", 2, "asdf" ]
// // }

// // bug cannot create the double nested array (55) -> output a flat array with everything
// // probablement bug dans JsonRuntime.CreateArray
// // {
// //     "a": [ "1", "asdf", 2, "asdf2", ["12", 13, [ "55"]] ]
// // }

// // case of dictionaries and arrays...
// // {
// //     "123": [ "10", 11 ],
// //     "456": []
// // }

// // crash ctor avec array optional qui plante. probably ctx.MakeOptionType(replaceJDocWithJValue ctx result.ConvertedType)
// // {
// //     "123": [ "a", "10", 11, [ "12", 13] ],
// //     "456": []
// // }

// // si on bypass le bug de multiplicity qui fait un optional array qui marche pas, on peut en faire 2 et tester que c'est ok on peut controler le original type des valeurs ou des nested int:
// // {
// //     "123": [ 30, "40", [ 10, 11 ], [ 123, 123] ],
// //     "456": []
// // }

// // plante
// {
//     "123": [ 30, "40", [ 10, 11 ] ],
//     "456": []
// }
//  """, PreferDictionaries=true>

// let sample = PT.GetSample()
// let firstValue = (sample[123])

// let write =
//     // PT.Root(PT.NumbersOrBooleansOrString([|1;2;3|], [|true;false|], "asdf"))
//     // PT.Root(PT.NumbersOrStringsOrArray([|1;2;3|], [|"asdf";"asdf2"|], [| 42|]))
//     // PT.Root(PT.NumbersOrStringsOrArray([|1;2;3|], [|"asdf";"asdf2"|], PT.NumbersOrArray([| 42 |], [|43|])))
//     PT.Root([ 123, PT.RootValue([| 42 |],  Some [| 1|])])
//     // PT.Root([|1;2;3|])

    


// let csv1 = CsvFile.Parse("a,b,c\n , ,1\na,b,2")
// csv1.Headers
// csv1.Rows |> Seq.head

// let csv = CsvProvider<"a,b,c\n , ,1\na,b,2",Schema=",string option,int option">.GetSample()
// let first = csv.Rows |> Seq.head
// first.A

// type PT = JsonProvider<"""{"k": [["foo", 1], ["bar", 2]] }""">
// // wrong inferred type:
// let k1 = PT.GetSample().K


// type OptionalString = JsonProvider<"""{"field": "asdf"} {}""", InferenceMode=InferenceMode.ValuesAndInlineSchemasOverrides, SampleIsList=true>
// type OptionalStringNullable = JsonProvider<"""{"field": "asdf"} {"field": null}""", InferenceMode=InferenceMode.ValuesAndInlineSchemasOverrides, SampleIsList=true>
// type StringEmpty = JsonProvider<"""{"field": "asdf"} {"field": ""}""", InferenceMode=InferenceMode.ValuesAndInlineSchemasOverrides, SampleIsList=true>
// type MandatoryString = JsonProvider<"""{"field": "asdf"} {"field": "asdf"}""", InferenceMode=InferenceMode.ValuesAndInlineSchemasOverrides, SampleIsList=true>
// OptionalString.Parse("""{"field":null}""").Field // None
// OptionalString.Parse("""{}""").Field // None
// OptionalStringNullable.Parse("""{"field":null}""").Field // None
// OptionalStringNullable.Parse("""{}""").Field // None
// StringEmpty.Parse("""{"field":null}""").Field // None
// StringEmpty.Parse("""{}""").Field // None
// MandatoryString.Parse("""{"field":null}""").Field // ""
// MandatoryString.Parse("""{}""").Field // ""




// // val t: JsonProvider<...>.Root = {
// //   "not": "valid"
// // }
// // val it: string = ""

// // System.Exception: '/field' is missing

// type MyXml = XmlProvider<"""<item field="typeof{float}" />""",InferenceMode=InferenceMode.ValuesAndInlineSchemasOverrides>
// let x = MyXml.Parse("""<item not="valid"/>""")
// x.Field
// // val x: XmlProvider<...>.Item = <item not="valid" />
// // val it: string = ""

// // System.Exception: Attribute field is missing

// type MyCsv = CsvProvider<"""A,B""", Schema="string,int?">
// let csvx = MyCsv.ParseRows("""1,""")
// csvx.[0].B

// type TestXml = XmlProvider<
//     """
//     <samples>
    
//     <root>
//         <error message="asdf" />
//     </root>
    
//     <root></root>
    
//     <root>
//         <data>
//             <item id="1"/>
//             <item id="2"/>
//         </data>
//     </root> 

//     <root>
//     <data/>
//     </root>

//     <!--
    
    
//         <item><test>typeof{int{metre}}</test></item>  
//         <item><test>typeof{bool}</test></item>
//         <item><test/></item>
//     <item/>
//     <item><test>asdf</test></item>  
//     <item><test>typeof{bool}</test></item>
    
//     <item><test>10900</test></item>       
//         <item><test/></item> 
//         <item attr="asdf"><test>typeof{string}</test></item>  
//         <item attr="1a"/>
//         <item attr="typeof{int}"/>   
//         <item attr="typeof{bool}"/>     
//         -->
    
//     </samples>
//     """
//     ,SampleIsList = true
//     // ,InferenceMode = InferenceMode.ValuesOnly
//     // ,InferenceMode = InferenceMode.ValuesAndInlineSchemasHints
//     ,InferenceMode = InferenceMode.ValuesAndInlineSchemasOverrides
//     >

// let testXml =
//     // TestXml.Parse("<root><item><test>12</test></item></root>")
//     // TestXml.Parse("""<item test="yes"><test>1</test></item>""")
//     (TestXml.GetSamples()[0])
//         // .Items[0].Attr.Boolean
//         .Data.Value.Items


// type TestXml2 = XmlProvider<
//     """
//     <test aa="1">
//         <child a="2" b="42"></child>
//     </test>
//     """
//     >

// let sample = TestXml2.GetSample()
// sample.XElement.GetHashCode()
// sample.Child.XElement.Parent.GetHashCode()

// sample.XElement
// sample.Child.XElement.SetValue("asdf")
// sample.Child.XElement.SetAttributeValue("a", "coucou")
// sample.Child.A

// let lensSetChildBAttribute (root:TestXml2.Test) value =
//     TestXml2.Test(aa=root.Aa,child=TestXml2.Child(a=root.Child.A,b=value))

// let changed = lensSetChildBAttribute sample 43
// changed.XElement.GetHashCode()
// changed.Child.XElement.Parent.GetHashCode()

// // let x = testXml()
// // let y = x  + 2<metre>

// let xxx = typeof<int<metre>>

// // type ServiceResponse = JsonProvider<"""[
// // { "code": 0, "value": {"generic payload": "yes"}, "message": null},
// // { "code": 1, "value": null, "message": "Warning"},
// // { "code": 2, "value": [], "message": "Exception"}   
// // ]
// // """
// // , SampleIsList = true
// // // ,InferenceMode = InferenceMode.ValuesOnly
// // // ,InferenceMode = InferenceMode.ValuesAndInlineSchemasHints
// // ,InferenceMode = InferenceMode.ValuesAndInlineSchemasOverrides>

// // let test2 =
// //     let root = ServiceResponse.GetSamples()[0]
// //     root.Value.Record.Value.GenericPayload

// type TestJson = JsonProvider<"""

//     {
//     "error": {
//         "message": "Error validating access token: Session has expired on Friday, 24-Jul-20 16:00:00 PDT. The current time is Friday, 24-Jul-20 16:06:14 PDT."
//     }
//     }

//     {
//     "data": [
//         {
//         "id": "17841511906082530"
//         },
//         {
//         "id": "17841511906082530"
//         }
//     ]
//     } 

//     // {
//     //   "data": [

//     //   ]
//     // } 
    
//     // {
//     //   "data": null
//     // } 
    
// """
//     // ,InferenceMode = InferenceMode.ValuesOnly
//     // ,InferenceMode = InferenceMode.ValuesAndInlineSchemasHints
//     ,InferenceMode = InferenceMode.ValuesAndInlineSchemasOverrides
//     ,SampleIsList = true >

// let testJson =
//     let item = (TestJson.GetSamples())[1]
//     item.Data

// let serializedJson =
//     TestJson.Root(
//         error = None, //Some (TestJson.Error(message = "asdf")),
//         data = [| TestJson.Datum(id = 1) |]
//     ).JsonValue.ToString()


// let mapOption f =
//     function
//     | None -> null
//     | Some v -> f v

// let array = System.Linq.Enumerable.ToArray([| 1; 2; 3 |]) :> Array
// let arrayOption = Some array
// let arrayToList = [| for elem in array -> elem |]
// let arrayOptionToList  = mapOption (fun (array: Array) -> [| for elem in array -> elem |]) arrayOption

// // let value = [| 1; 2; 3|] |> Some |> box

// // let optionType =
// //   let optionType = value.GetType()
// //   let optionTypeArg = optionType.GetGenericArguments()[0]
// //   let info, fields = FSharp.Reflection.FSharpValue.GetUnionFields(value, optionType) 
// //   match info.Name, fields with
// //   | "Some", [| x |] -> Some (unbox x : Array)
// //   | "None", _ -> None
// //   | x, _ -> failwith x

// // match value with
// // | :? Array as v -> "array"
// // | :? option<Array> as v -> "array option"
// // | :? option<_> as v -> "array option ob"
// // | :? option<'a> as v ->
// //   match v with
// //   | None -> "None option array"
// //   | Some v ->
// //     [for x in v -> ""] |> ignore
// //     "does it work?"
// // | typ when typ <> null && typ.GetType().IsGenericType -> "ok"
// // | _ -> failwith $"fail {value.GetType()}"

// // $"{(typedefof<option<_>>).IsConstructedGenericType }"
// // $"{ value.GetType().GetGenericTypeDefinition() = (typedefof<option<_>>) }"
// // $"{ (value.GetType().GetGenericArguments()[0]).BaseType = (typeof<Array>)  }"

// // type TwitterJson =
// //     JsonProvider<
// //         @"D:\Data\Dev\Archival-Mess\FSharp.Data\tests\FSharp.Data.Tests\Data\TwitterStream.json"
// //         , SampleIsList = true
// //         , InferTypesFromValues = true
// //         , PreferDictionaries = false>

// // let test =
// //     let user = 
// //         TwitterJson.GetSamples()
// //             .[0].User
// //     let a = user.Value.ProfileLinkColor
// //     let b = user.Value.ProfileTextColor
// //     let c = user.Value.ProfileBackgroundColor
// //     let d = user.Value.ProfileSidebarFillColor
// //     let e = user.Value.ProfileSidebarBorderColor
// //     a,b,c,d,e
