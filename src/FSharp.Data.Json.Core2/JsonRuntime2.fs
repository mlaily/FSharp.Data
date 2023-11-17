// --------------------------------------------------------------------------------------
// JSON type provider - methods that are called from the generated erased code
// --------------------------------------------------------------------------------------

namespace FSharp.Data.Runtime

open System
open System.Globalization
open FSharp.Data
open FSharp.Data.Runtime
open FSharp.Data.Runtime.BaseTypes
open FSharp.Data.Runtime.StructuralTypes
open System.Text.Json.Nodes
open System.Text.Json

///// <exclude />
//type JsonValueOptionAndPath2 =
//    { JsonOpt: JsonValue2 option
//      Path: string }

/// Static helper methods called from the generated code for working with JSON
type JsonRuntime2 =
    static member private FormatJsonForException(json: string) =
        if isNull json then
            "`null`"
        else
            let truncatedJson = json.Substring(0, Math.Min(100, json.Length))
            let truncationMark = if truncatedJson.Length = json.Length then "" else " [...]"
            $"`{truncatedJson}{truncationMark}`"

    static member GetPrimitiveValue(json: JsonWrapper) =
        match json.JsonNode with
        | null ->
            failwith $"Trying to get primitive value: expected node to be a {nameof Nodes.JsonValue} but was `null`."
        | :? Nodes.JsonValue as value ->
            let mutable result = Unchecked.defaultof<_>

            if value.TryGetValue<'TValue>(&result) then
                result
            else
                try
                    // Deserialize() is required when the node was manually created and is not backed by a JsonElement but by a concrete type.
                    // Unfortunately if this fails the exception doesn't contain the full json path, so we have to catch it and rethrow to have it...
                    value.Deserialize<'TValue>()
                with ex ->
                    raise (
                        Exception(
                            $"Could not convert JsonValue into a {typeof<'TValue>} at path {value.GetPath()}. Node value: {JsonRuntime2.FormatJsonForException(value.ToJsonString())}",
                            ex
                        )
                    )
        | other ->
            failwith
                $"Trying to get primitive value at {json.JsonNode.GetPath()}: expected node to be a {nameof Nodes.JsonValue} but was {other.GetType()}. Node value: {JsonRuntime2.FormatJsonForException(other.ToJsonString())}"

    static member GetRecordProperty(json: JsonWrapper, propertyName) =
        match json.JsonNode with
        | null ->
            failwith
                $"Trying to get property \"{propertyName}\": expected node to be a {nameof JsonObject} but was `null`."
        | :? JsonObject as obj ->
            let mutable result = Unchecked.defaultof<_>

            if obj.TryGetPropertyValue(propertyName, &result) then
                JsonWrapper(result)
            else
                failwith
                    $"Property {propertyName} does not exist at {json.JsonNode.GetPath()}. Node value: {JsonRuntime2.FormatJsonForException(json.JsonNode.ToJsonString())}"
        | other ->
            failwith
                $"Trying to get property \"{propertyName}\": expected node at {json.JsonNode.GetPath()} to be a {nameof JsonObject} but was {other.GetType()}. Node value: {JsonRuntime2.FormatJsonForException(other.ToJsonString())}"

    static member GetArrayValues(json: JsonWrapper, convertItem: Func<JsonWrapper, 'TValue>) =
        match json.JsonNode with
        | null -> failwith $"Trying to get array: expected node to be a {nameof JsonArray} but was `null`."
        | :? JsonArray as array ->
            [| for item in array ->
                   try
                       JsonWrapper(item) |> convertItem.Invoke
                   with ex ->
                       if isNull item then
                           raise (
                               Exception(
                                   failwith
                                       $"Could not convert JsonArray into values of type {typeof<'TValue>} at path {array.GetPath()} because one of the items was `null`. Array node value: {JsonRuntime2.FormatJsonForException(array.ToJsonString())}",
                                   ex
                               )
                           )
                       else
                           raise (
                               Exception(
                                   $"Could not convert JsonArray into values of type {typeof<'TValue>} at path {item.GetPath()}. Item node value: {JsonRuntime2.FormatJsonForException(item.ToJsonString())}",
                                   ex
                               )
                           ) |]
        | other ->
            failwith
                $"Trying to get array: expected node at {json.JsonNode.GetPath()} to be a {nameof JsonArray} but was {other.GetType()}. Node value: {JsonRuntime2.FormatJsonForException(other.ToJsonString())}"


    /// Heterogeneous types are erased to a JsonWrapper option, so that's what we take here.
    static member GetHeterogeneousValue(jsonOpt: JsonWrapper option, convertItem: Func<JsonWrapper, 'TValue>) =
        match jsonOpt with
        | None -> None
        | Some json ->
            try
                json |> convertItem.Invoke |> Some
            with ex ->
                None
    //if isNull (json.JsonNode) then raise (Exception(failwith $"Could not convert node into a value of type {typeof<'TValue>} (the node was `null`).", ex))
    //else raise (Exception($"Could not convert node into a value of type {typeof<'TValue>} at path {json.JsonNode.GetPath()}. Node value: {JsonRuntime2.FormatJsonForException(json.JsonNode.ToJsonString())}", ex))

    static member GetAsOptional(json: JsonWrapper (*, mapping: Func<JsonWrapper, 'TValue>*) ) =
        match json.JsonNode with
        | null -> None
        | x -> x |> JsonWrapper |> Some
    //|> Option.map (fun value -> JsonWrapper(value))
    //|> Option.map mapping.Invoke

    static member GetPrimitiveValueAsOptional(json: JsonWrapper) =
        JsonRuntime2.GetAsOptional(json)
        |> Option.map (fun x -> JsonRuntime2.GetPrimitiveValue(x))

    static member GetArrayValuesAsOptional(json: JsonWrapper, convertItem: Func<JsonWrapper, 'TValue>) =
        JsonRuntime2.GetAsOptional(json)
        |> Option.map (fun x -> JsonRuntime2.GetArrayValues(x, convertItem))


// --------------------------------------------------------------------------------------
// json option -> type

//static member ConvertString(cultureStr, json) =
//    json
//    |> Option.bind (JsonConversions2.AsString (TextRuntime.GetCulture cultureStr))

//static member ConvertInteger(cultureStr, json) =
//    json
//    |> Option.bind (JsonConversions2.AsInteger(TextRuntime.GetCulture cultureStr))

//static member ConvertInteger64(cultureStr, json) =
//    json
//    |> Option.bind (JsonConversions2.AsInteger64(TextRuntime.GetCulture cultureStr))

//static member ConvertDecimal(cultureStr, json) =
//    json
//    |> Option.bind (JsonConversions2.AsDecimal(TextRuntime.GetCulture cultureStr))

//static member ConvertFloat(cultureStr, json) =
//    json
//    |> Option.bind (JsonConversions2.AsFloat(TextRuntime.GetCulture cultureStr))

//static member ConvertBoolean(json) =
//    json |> Option.bind JsonConversions2.AsBoolean

//static member ConvertDateTimeOffset(cultureStr, json) =
//    json
//    |> Option.bind (JsonConversions2.AsDateTimeOffset(TextRuntime.GetCulture cultureStr))

//static member ConvertDateTime(cultureStr, json) =
//    json
//    |> Option.bind (JsonConversions2.AsDateTime(TextRuntime.GetCulture cultureStr))

//static member ConvertTimeSpan(cultureStr, json) =
//    json
//    |> Option.bind (JsonConversions2.AsTimeSpan(TextRuntime.GetCulture cultureStr))

//static member ConvertGuid(json) =
//    json |> Option.bind JsonConversions2.AsGuid

///// Operation that extracts the value from an option and reports a meaningful error message when the value is not there
//static member GetNonOptionalValue<'T>(path: string, opt: option<'T>, originalValue) : 'T =
//    let getTypeName () =
//        let name = typeof<'T>.Name

//        if name.StartsWith("i", StringComparison.OrdinalIgnoreCase) then
//            "an " + name
//        else
//            "a " + name

//    match opt, originalValue with
//    | Some value, _ -> value
//    | None,
//      Some ((JsonValue2.Array _
//      | JsonValue2.Record _) as x) ->
//        failwithf "Expecting %s at '%s', got %s" (getTypeName ()) path
//        <| x.ToString(JsonSaveOptions2.DisableFormatting)
//    | None, None -> failwithf "'%s' is missing" path
//    | None, Some x ->
//        failwithf "Expecting %s at '%s', got %s" (getTypeName ()) path
//        <| x.ToString(JsonSaveOptions2.DisableFormatting)

///// Converts JSON array to array of target types
//static member ConvertArray<'T>(doc: IJsonDocument2, mapping: Func<IJsonDocument2, 'T>) =
//    match doc.JsonValue with
//    | JsonValue2.Array elements ->
//        elements
//        |> Array.mapi (fun i value ->
//            doc.CreateNew(value, "[" + (string i) + "]")
//            |> mapping.Invoke)
//    | JsonValue2.Null -> [||]
//    | x ->
//        failwithf "Expecting an array at '%s', got %s" (doc.Path())
//        <| x.ToString(JsonSaveOptions2.DisableFormatting)

//static member TryConvertArray<'T>(doc: IJsonDocument2, mapping: Func<IJsonDocument2, 'T>) =
//    match doc.JsonValue with
//    | JsonValue2.Array elements ->
//        elements
//        |> Array.mapi (fun i value ->
//            doc.CreateNew(value, "[" + (string i) + "]")
//            |> mapping.Invoke)
//        |> Some
//    | JsonValue2.Null -> None
//    | x ->
//        failwithf "Expecting an array at '%s', got %s" (doc.Path())
//        <| x.ToString(JsonSaveOptions2.DisableFormatting)

///// Get properties of the record
//static member GetRecordProperties(doc: IJsonDocument2) =
//    match doc.JsonValue with
//    | JsonValue2.Record items -> items
//    | JsonValue2.Null -> [||]
//    | x ->
//        failwithf "Expecting a record at '%s', got %s" (doc.Path())
//        <| x.ToString(JsonSaveOptions2.DisableFormatting)

///// Converts JSON record to dictionary
//static member ConvertRecordToDictionary<'Key, 'Value when 'Key: equality>
//    (
//        doc: IJsonDocument2,
//        mappingKey: Func<IJsonDocument2, 'Key>,
//        mappingValue: Func<IJsonDocument2, 'Value>
//    ) =
//    JsonRuntime2.GetRecordProperties(doc)
//    |> Seq.map (fun (k, v) ->
//        let key =
//            doc.CreateNew(JsonValue2.String k, k)
//            |> mappingKey.Invoke

//        let value = doc.CreateNew(v, k) |> mappingValue.Invoke
//        key, value)


///// Get a value by the key from infered dictionary
//static member InferedDictionaryContainsKey<'Key when 'Key: equality>
//    (
//        doc: IJsonDocument2,
//        mappingKey: Func<IJsonDocument2, 'Key>,
//        key: 'Key
//    ) =
//    let finder (k, _) =
//        (doc.CreateNew(JsonValue2.String k, k)
//         |> mappingKey.Invoke) = key

//    (JsonRuntime2.GetRecordProperties(doc)
//     |> Array.tryFind finder)
//        .IsSome

///// Try get a value by the key from infered dictionary
//static member TryGetValueByKeyFromInferedDictionary<'Key, 'Value when 'Key: equality>
//    (
//        doc: IJsonDocument2,
//        mappingKey: Func<IJsonDocument2, 'Key>,
//        mappingValue: Func<IJsonDocument2, 'Value>,
//        key: 'Key
//    ) =
//    let picker (k, v) =
//        if (doc.CreateNew(JsonValue2.String k, k)
//            |> mappingKey.Invoke) = key then
//            doc.CreateNew(v, k) |> mappingValue.Invoke |> Some
//        else
//            None

//    JsonRuntime2.GetRecordProperties(doc)
//    |> Array.tryPick picker

///// Get a value by the key from infered dictionary
//static member GetValueByKeyFromInferedDictionary<'Key, 'Value when 'Key: equality>
//    (
//        doc: IJsonDocument2,
//        mappingKey: Func<IJsonDocument2, 'Key>,
//        mappingValue: Func<IJsonDocument2, 'Value>,
//        key: 'Key
//    ) =
//    match JsonRuntime2.TryGetValueByKeyFromInferedDictionary(doc, mappingKey, mappingValue, key) with
//    | Some value -> value
//    | _ ->
//        key
//        |> sprintf "The given key '%A' was not present in the dictionary."
//        |> System.Collections.Generic.KeyNotFoundException
//        |> raise

///// Get keys from infered dictionary
//static member GetKeysFromInferedDictionary<'Key when 'Key: equality>
//    (
//        doc: IJsonDocument2,
//        mappingKey: Func<IJsonDocument2, 'Key>
//    ) =
//    JsonRuntime2.GetRecordProperties(doc)
//    |> Array.map (fun (k, _) ->
//        doc.CreateNew(JsonValue2.String k, k)
//        |> mappingKey.Invoke)

///// Get values from infered dictionary
//static member GetValuesFromInferedDictionary<'Value>
//    (
//        doc: IJsonDocument2,
//        mappingValue: Func<IJsonDocument2, 'Value>
//    ) =
//    JsonRuntime2.GetRecordProperties(doc)
//    |> Array.map (fun (k, v) -> doc.CreateNew(v, k) |> mappingValue.Invoke)

///// Get optional json property
//static member TryGetPropertyUnpacked(doc: IJsonDocument2, name) =
//    match doc.JsonValue with
//        | JsonValue2.Record properties ->
//            Array.tryFind (fst >> (=) name) properties
//            |> Option.map snd
//        | _ -> None
//    // for heterogeneous types we want to be able to differentiate between missing value and null, so we want the null here
//    // TODO: review usages to make sure it doesn't break something else...
//    //|> Option.bind (function
//    //    | JsonValue2.Null -> None
//    //    | x -> Some x)

///// Get optional json property and wrap it together with path
//static member TryGetPropertyUnpackedWithPath(doc: IJsonDocument2, name) =
//    { JsonOpt = JsonRuntime2.TryGetPropertyUnpacked(doc, name)
//      Path = doc.Path() + "/" + name }

///// Get optional json property wrapped in json document
//static member TryGetPropertyPacked(doc: IJsonDocument2, name) =
//    JsonRuntime2.TryGetPropertyUnpacked(doc, name)
//    |> Option.map (fun value -> doc.CreateNew(value, "/" + name))

///// Get json property and wrap in json document
//static member GetPropertyPacked(doc: IJsonDocument2, name) =
//    match JsonRuntime2.TryGetPropertyPacked(doc, name) with
//    | Some doc -> doc
//    | None ->
//        failwithf "Property '%s' not found at '%s': %s" name (doc.Path())
//        <| doc.JsonValue.ToString(JsonSaveOptions2.DisableFormatting)

///// Get json property and wrap in json document, and return null if not found
//static member GetPropertyPackedOrNull(doc: IJsonDocument2, name) =
//    match JsonRuntime2.TryGetPropertyPacked(doc, name) with
//    | Some doc -> doc
//    | None -> doc.CreateNew(JsonValue2.Null, "/" + name)

///// Get optional json property and convert to a specified type
//static member ConvertOptionalProperty<'T>(doc: IJsonDocument2, name, mapping: Func<IJsonDocument2, 'T>) =
//    JsonRuntime2.TryGetPropertyPacked(doc, name)
//    |> Option.map mapping.Invoke

//static member ConvertOptionalValue<'T>(doc: IJsonDocument2, mapping: Func<IJsonDocument2, 'T>) =
//    match doc.JsonValue with
//    | JsonValue2.Null -> None
//    | x -> Some x
//    |> Option.map (fun value -> doc.CreateNew(value, "/"))
//    |> Option.map mapping.Invoke

//static member private Matches cultureStr tag =
//    match tag with
//    | InferedTypeTag.Number ->
//        let cultureInfo = TextRuntime.GetCulture cultureStr

//        fun json ->
//            (JsonConversions2.AsDecimal cultureInfo json).IsSome
//            || (JsonConversions2.AsFloat cultureInfo json).IsSome
//    | InferedTypeTag.Boolean -> JsonConversions2.AsBoolean >> Option.isSome
//    | InferedTypeTag.String ->
//        JsonConversions2.AsString (TextRuntime.GetCulture cultureStr)
//        >> Option.isSome
//    | InferedTypeTag.DateTime ->
//        let cultureInfo = TextRuntime.GetCulture cultureStr

//        fun json ->
//            (JsonConversions2.AsDateTimeOffset cultureInfo json).IsSome
//            || (JsonConversions2.AsDateTime cultureInfo json).IsSome
//    | InferedTypeTag.DateTimeOffset ->
//        let cultureInfo = TextRuntime.GetCulture cultureStr
//        fun json -> (JsonConversions2.AsDateTimeOffset cultureInfo json).IsSome
//    | InferedTypeTag.TimeSpan ->
//        JsonConversions2.AsTimeSpan(TextRuntime.GetCulture cultureStr)
//        >> Option.isSome
//    | InferedTypeTag.Guid -> JsonConversions2.AsGuid >> Option.isSome
//    | InferedTypeTag.Collection ->
//        function
//        | JsonValue2.Array _ -> true
//        | _ -> false
//    | InferedTypeTag.Record _ ->
//        function
//        | JsonValue2.Record _ -> true
//        | _ -> false
//    | InferedTypeTag.Json -> failwith "Json type not supported"
//    | InferedTypeTag.Null -> failwith "Null type not supported"
//    | InferedTypeTag.Heterogeneous -> failwith "Heterogeneous type not supported"

///// Returns a single or no value by tag type
//static member TryGetValueByTypeTag<'T>(doc: IJsonDocument2 option, cultureStr, tagCode, mapping: Func<IJsonDocument2, 'T>) =
//    doc
//    |> Option.bind (fun doc ->
//        if JsonRuntime2.Matches cultureStr (InferedTypeTag.ParseCode tagCode) doc.JsonValue then
//            Some(mapping.Invoke doc)
//        else
//            None)

//static member private ToJsonValue2 (cultureInfo: CultureInfo) (originalType: PrimitiveType option) (value: obj) =
//    let inline optionToJson f opt =
//        match opt with
//        | None -> JsonValue2.Null
//        | Some v -> f v

//    let inline arrayOptionToJson opt =
//        match opt with
//        | None -> JsonValue2.Null
//        | Some v -> JsonValue2.Array [| for elem in v -> JsonRuntime2.ToJsonValue2 cultureInfo originalType elem |]

//    let inline intToString (x: int) = x.ToString(cultureInfo)
//    let inline int64ToString (x: int64) = x.ToString(cultureInfo)
//    let inline floatToString (x: float) = x.ToString(cultureInfo)
//    let inline decimalToString (x: decimal) = x.ToString(cultureInfo)
//    let inline boolToDecimal x = if x then 1m else 0m
//    let inline boolToString x = if x then "true" else "false"

//    let inline serializeInt originalType (x: int) =
//        match originalType with
//        | Some PrimitiveType.String -> x |> intToString |> JsonValue2.String
//        | _ -> x |> decimal |> JsonValue2.Number

//    let inline serializeInt64 originalType (x: int64) =
//        match originalType with
//        | Some PrimitiveType.String -> x |> int64ToString |> JsonValue2.String
//        | _ -> x |> decimal |> JsonValue2.Number

//    let inline serializeFloat originalType (x: float) =
//        match originalType with
//        | Some PrimitiveType.String -> x |> floatToString |> JsonValue2.String
//        | _ -> x |> decimal |> JsonValue2.Number

//    let inline serializeDecimal originalType (x: decimal) =
//        match originalType with
//        | Some PrimitiveType.String -> x |> decimalToString |> JsonValue2.String
//        | _ -> x |> JsonValue2.Number

//    let inline serializeBool originalType (x: bool) =
//        match originalType with
//        | Some PrimitiveType.Number -> x |> boolToDecimal |> JsonValue2.Number
//        | Some PrimitiveType.String -> x |> boolToString |> JsonValue2.String
//        | _ -> x |> JsonValue2.Boolean

//    match value with
//    | null -> JsonValue2.Null
//    | :? Array as v -> JsonValue2.Array [| for elem in v -> JsonRuntime2.ToJsonValue2 cultureInfo originalType elem |]

//    | :? string as v -> JsonValue2.String v
//    | :? DateTime as v -> v.ToString("O", cultureInfo) |> JsonValue2.String
//    | :? DateTimeOffset as v -> v.ToString("O", cultureInfo) |> JsonValue2.String
//    | :? TimeSpan as v -> v.ToString("g", cultureInfo) |> JsonValue2.String
//    | :? int as v -> serializeInt originalType v
//    | :? int64 as v -> serializeInt64 originalType v
//    | :? float as v -> serializeFloat originalType v
//    | :? decimal as v -> serializeDecimal originalType v
//    | :? bool as v -> serializeBool originalType v
//    | :? Guid as v -> v.ToString() |> JsonValue2.String
//    | :? IJsonDocument2 as v -> v.JsonValue
//    | :? JsonValue2 as v -> v

//    | :? option<string> as v -> optionToJson JsonValue2.String v
//    | :? option<DateTime> as v ->
//        optionToJson (fun (dt: DateTime) -> dt.ToString(cultureInfo) |> JsonValue2.String) v
//    | :? option<DateTimeOffset> as v ->
//        optionToJson (fun (dt: DateTimeOffset) -> dt.ToString(cultureInfo) |> JsonValue2.String) v
//    | :? option<TimeSpan> as v ->
//        optionToJson (fun (ts: TimeSpan) -> ts.ToString("g", cultureInfo) |> JsonValue2.String) v
//    | :? option<int> as v -> optionToJson (serializeInt originalType) v
//    | :? option<int64> as v -> optionToJson (serializeInt64 originalType) v
//    | :? option<float> as v -> optionToJson (serializeFloat originalType) v
//    | :? option<decimal> as v -> optionToJson (serializeDecimal originalType) v
//    | :? option<bool> as v -> optionToJson (serializeBool originalType) v
//    | :? option<Guid> as v -> optionToJson (fun (g: Guid) -> g.ToString() |> JsonValue2.String) v
//    | :? option<IJsonDocument2> as v -> optionToJson (fun (v: IJsonDocument2) -> v.JsonValue) v
//    | :? option<JsonValue2> as v -> optionToJson id v

//    | :? option<string[]> as v -> arrayOptionToJson v
//    | :? option<DateTime[]> as v -> arrayOptionToJson v
//    | :? option<DateTimeOffset[]> as v -> arrayOptionToJson v
//    | :? option<TimeSpan[]> as v -> arrayOptionToJson v
//    | :? option<int[]> as v -> arrayOptionToJson v
//    | :? option<int64[]> as v -> arrayOptionToJson v
//    | :? option<float[]> as v -> arrayOptionToJson v
//    | :? option<decimal[]> as v -> arrayOptionToJson v
//    | :? option<bool[]> as v -> arrayOptionToJson v
//    | :? option<Guid[]> as v -> arrayOptionToJson v
//    | :? option<IJsonDocument2[]> as v -> arrayOptionToJson v
//    | :? option<JsonValue2[]> as v -> arrayOptionToJson v

//    | _ -> failwithf "Can't create JsonValue2 from %A" value

///// Creates a scalar JsonValue2 and wraps it in a json document
//static member CreateValue(value: obj, originalType: int, cultureStr) =
//    let cultureInfo = TextRuntime.GetCulture cultureStr
//    let json = JsonRuntime2.ToJsonValue2 cultureInfo (originalType |> PrimitiveType.FromInt) value
//    JsonDocument2.Create(json, "")

//// Creates a JsonValue2.Record and wraps it in a json document
//static member CreateRecord(properties, cultureStr) =
//    let cultureInfo = TextRuntime.GetCulture cultureStr

//    let json =
//        properties
//        |> Array.choose (fun (k, v: obj, originalType, doNotWriteNulls) ->
//            let jvalue = JsonRuntime2.ToJsonValue2 cultureInfo (originalType |> PrimitiveType.FromInt) v
//            if doNotWriteNulls && jvalue = JsonValue2.Null then None
//            else Some (k, jvalue))
//        |> JsonValue2.Record

//    JsonDocument2.Create(json, "")

//// Creates a JsonValue2.Record from key*value seq and wraps it in a json document
//static member CreateRecordFromDictionary<'Key, 'Value when 'Key: equality>
//    (
//        keyValuePairs: ('Key * 'Value) seq,
//        cultureStr,
//        mappingKeyBack: Func<'Key, string>,
//        originalValueType
//    ) =
//    let cultureInfo = TextRuntime.GetCulture cultureStr

//    let json =
//        keyValuePairs
//        |> Seq.map (fun (k, v) ->
//            (k |> mappingKeyBack.Invoke),
//            JsonRuntime2.ToJsonValue2 cultureInfo (originalValueType |> PrimitiveType.FromInt) (v :> obj))
//        |> Seq.toArray
//        |> JsonValue2.Record

//    JsonDocument2.Create(json, "")

///// Creates a scalar JsonValue2.Array and wraps it in a json document
///// elements is actually an obj[][]: an array of all the user-provided arrays from a ctor
///// (e.g [ [1;2;3] ; ["a";"b";"c"]  ] in the case of an array inferred to contain IntsOrStrings)
//static member CreateArray(elements, cultureStr) =
//    let cultureInfo = TextRuntime.GetCulture cultureStr

//    let json =
//        elements
//        |> Array.map (fun (array: obj, originalType) ->
//            JsonRuntime2.ToJsonValue2 cultureInfo (originalType |> PrimitiveType.FromInt) array)
//        |> Array.collect (function
//            | JsonValue2.Array elements -> elements
//            | JsonValue2.Null -> [||]
//            | element -> [| element |])
//        |> JsonValue2.Array

//    JsonDocument2.Create(json, "")
