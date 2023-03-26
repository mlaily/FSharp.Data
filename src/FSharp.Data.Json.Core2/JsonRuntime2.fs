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

/// <exclude />
type JsonValueOptionAndPath2 =
    { JsonOpt: JsonValue2 option
      Path: string }

/// Static helper methods called from the generated code for working with JSON
type JsonRuntime2 =

    // --------------------------------------------------------------------------------------
    // json option -> type

    static member ConvertString(cultureStr, json) =
        json
        |> Option.bind (JsonConversions2.AsString true (TextRuntime.GetCulture cultureStr))

    static member ConvertInteger(cultureStr, json) =
        json
        |> Option.bind (JsonConversions2.AsInteger(TextRuntime.GetCulture cultureStr))

    static member ConvertInteger64(cultureStr, json) =
        json
        |> Option.bind (JsonConversions2.AsInteger64(TextRuntime.GetCulture cultureStr))

    static member ConvertDecimal(cultureStr, json) =
        json
        |> Option.bind (JsonConversions2.AsDecimal(TextRuntime.GetCulture cultureStr))

    static member ConvertFloat(cultureStr, missingValuesStr, json) =
        json
        |> Option.bind (
            JsonConversions2.AsFloat
                (TextRuntime.GetMissingValues missingValuesStr)
                true
                (TextRuntime.GetCulture cultureStr)
        )

    static member ConvertBoolean(json) =
        json |> Option.bind JsonConversions2.AsBoolean

    static member ConvertDateTimeOffset(cultureStr, json) =
        json
        |> Option.bind (JsonConversions2.AsDateTimeOffset(TextRuntime.GetCulture cultureStr))

    static member ConvertDateTime(cultureStr, json) =
        json
        |> Option.bind (JsonConversions2.AsDateTime(TextRuntime.GetCulture cultureStr))

    static member ConvertTimeSpan(cultureStr, json) =
        json
        |> Option.bind (JsonConversions2.AsTimeSpan(TextRuntime.GetCulture cultureStr))

    static member ConvertGuid(json) =
        json |> Option.bind JsonConversions2.AsGuid

    /// Operation that extracts the value from an option and reports a meaningful error message when the value is not there
    /// If the originalValue is a scalar, for missing strings we return "", and for missing doubles we return NaN
    /// For other types an error is thrown
    static member GetNonOptionalValue<'T>(path: string, opt: option<'T>, originalValue) : 'T =
        let getTypeName () =
            let name = typeof<'T>.Name

            if name.StartsWith("i", StringComparison.OrdinalIgnoreCase) then
                "an " + name
            else
                "a " + name

        match opt, originalValue with
        | Some value, _ -> value
        | None,
          Some ((JsonValue2.Array _
          | JsonValue2.Record _) as x) ->
            failwithf "Expecting %s at '%s', got %s" (getTypeName ()) path
            <| x.ToString(JsonSaveOptions2.DisableFormatting)
        | None, _ when typeof<'T> = typeof<string> -> "" |> unbox
        | None, _ when typeof<'T> = typeof<float> -> Double.NaN |> unbox
        | None, None -> failwithf "'%s' is missing" path
        | None, Some x ->
            failwithf "Expecting %s at '%s', got %s" (getTypeName ()) path
            <| x.ToString(JsonSaveOptions2.DisableFormatting)

    /// Converts JSON array to array of target types
    static member ConvertArray<'T>(doc: IJsonDocument2, mapping: Func<IJsonDocument2, 'T>) =
        match doc.JsonValue with
        | JsonValue2.Array elements ->
            elements
            |> Array.filter (function
                | JsonValue2.Null -> false
                | JsonValue2.String s when s |> TextConversions.AsString |> Option.isNone -> false
                | _ -> true)
            |> Array.mapi (fun i value ->
                doc.CreateNew(value, "[" + (string i) + "]")
                |> mapping.Invoke)
        | JsonValue2.Null -> [||]
        | x ->
            failwithf "Expecting an array at '%s', got %s" (doc.Path())
            <| x.ToString(JsonSaveOptions2.DisableFormatting)

    /// Get properties of the record
    static member GetRecordProperties(doc: IJsonDocument2) =
        match doc.JsonValue with
        | JsonValue2.Record items -> items
        | JsonValue2.Null -> [||]
        | x ->
            failwithf "Expecting a record at '%s', got %s" (doc.Path())
            <| x.ToString(JsonSaveOptions2.DisableFormatting)

    /// Converts JSON record to dictionary
    static member ConvertRecordToDictionary<'Key, 'Value when 'Key: equality>
        (
            doc: IJsonDocument2,
            mappingKey: Func<IJsonDocument2, 'Key>,
            mappingValue: Func<IJsonDocument2, 'Value>
        ) =
        JsonRuntime2.GetRecordProperties(doc)
        |> Seq.map (fun (k, v) ->
            let key =
                doc.CreateNew(JsonValue2.String k, k)
                |> mappingKey.Invoke

            let value = doc.CreateNew(v, k) |> mappingValue.Invoke
            key, value)


    /// Get a value by the key from infered dictionary
    static member InferedDictionaryContainsKey<'Key when 'Key: equality>
        (
            doc: IJsonDocument2,
            mappingKey: Func<IJsonDocument2, 'Key>,
            key: 'Key
        ) =
        let finder (k, _) =
            (doc.CreateNew(JsonValue2.String k, k)
             |> mappingKey.Invoke) = key

        (JsonRuntime2.GetRecordProperties(doc)
         |> Array.tryFind finder)
            .IsSome

    /// Try get a value by the key from infered dictionary
    static member TryGetValueByKeyFromInferedDictionary<'Key, 'Value when 'Key: equality>
        (
            doc: IJsonDocument2,
            mappingKey: Func<IJsonDocument2, 'Key>,
            mappingValue: Func<IJsonDocument2, 'Value>,
            key: 'Key
        ) =
        let picker (k, v) =
            if (doc.CreateNew(JsonValue2.String k, k)
                |> mappingKey.Invoke) = key then
                doc.CreateNew(v, k) |> mappingValue.Invoke |> Some
            else
                None

        JsonRuntime2.GetRecordProperties(doc)
        |> Array.tryPick picker

    /// Get a value by the key from infered dictionary
    static member GetValueByKeyFromInferedDictionary<'Key, 'Value when 'Key: equality>
        (
            doc: IJsonDocument2,
            mappingKey: Func<IJsonDocument2, 'Key>,
            mappingValue: Func<IJsonDocument2, 'Value>,
            key: 'Key
        ) =
        match JsonRuntime2.TryGetValueByKeyFromInferedDictionary(doc, mappingKey, mappingValue, key) with
        | Some value -> value
        | _ ->
            key
            |> sprintf "The given key '%A' was not present in the dictionary."
            |> System.Collections.Generic.KeyNotFoundException
            |> raise

    /// Get keys from infered dictionary
    static member GetKeysFromInferedDictionary<'Key when 'Key: equality>
        (
            doc: IJsonDocument2,
            mappingKey: Func<IJsonDocument2, 'Key>
        ) =
        JsonRuntime2.GetRecordProperties(doc)
        |> Array.map (fun (k, _) ->
            doc.CreateNew(JsonValue2.String k, k)
            |> mappingKey.Invoke)

    /// Get values from infered dictionary
    static member GetValuesFromInferedDictionary<'Value>
        (
            doc: IJsonDocument2,
            mappingValue: Func<IJsonDocument2, 'Value>
        ) =
        JsonRuntime2.GetRecordProperties(doc)
        |> Array.map (fun (k, v) -> doc.CreateNew(v, k) |> mappingValue.Invoke)

    /// Get optional json property
    static member TryGetPropertyUnpacked(doc: IJsonDocument2, name) =
        doc.JsonValue.TryGetProperty(name)
        |> Option.bind (function
            | JsonValue2.Null
            | JsonValue2.String "" -> None
            | x -> Some x)

    /// Get optional json property and wrap it together with path
    static member TryGetPropertyUnpackedWithPath(doc: IJsonDocument2, name) =
        { JsonOpt = JsonRuntime2.TryGetPropertyUnpacked(doc, name)
          Path = doc.Path() + "/" + name }

    /// Get optional json property wrapped in json document
    static member TryGetPropertyPacked(doc: IJsonDocument2, name) =
        JsonRuntime2.TryGetPropertyUnpacked(doc, name)
        |> Option.map (fun value -> doc.CreateNew(value, "/" + name))

    /// Get json property and wrap in json document
    static member GetPropertyPacked(doc: IJsonDocument2, name) =
        match JsonRuntime2.TryGetPropertyPacked(doc, name) with
        | Some doc -> doc
        | None ->
            failwithf "Property '%s' not found at '%s': %s" name (doc.Path())
            <| doc.JsonValue.ToString(JsonSaveOptions2.DisableFormatting)

    /// Get json property and wrap in json document, and return null if not found
    static member GetPropertyPackedOrNull(doc: IJsonDocument2, name) =
        match JsonRuntime2.TryGetPropertyPacked(doc, name) with
        | Some doc -> doc
        | None -> doc.CreateNew(JsonValue2.Null, "/" + name)

    /// Get optional json property and convert to a specified type
    static member ConvertOptionalProperty<'T>(doc: IJsonDocument2, name, mapping: Func<IJsonDocument2, 'T>) =
        JsonRuntime2.TryGetPropertyPacked(doc, name)
        |> Option.map mapping.Invoke

    static member private Matches cultureStr tag =
        match tag with
        | InferedTypeTag.Number ->
            let cultureInfo = TextRuntime.GetCulture cultureStr

            fun json ->
                (JsonConversions2.AsDecimal cultureInfo json).IsSome
                || (JsonConversions2.AsFloat [||] true cultureInfo json).IsSome
        | InferedTypeTag.Boolean -> JsonConversions2.AsBoolean >> Option.isSome
        | InferedTypeTag.String ->
            JsonConversions2.AsString true (TextRuntime.GetCulture cultureStr)
            >> Option.isSome
        | InferedTypeTag.DateTime ->
            let cultureInfo = TextRuntime.GetCulture cultureStr

            fun json ->
                (JsonConversions2.AsDateTimeOffset cultureInfo json).IsSome
                || (JsonConversions2.AsDateTime cultureInfo json).IsSome
        | InferedTypeTag.DateTimeOffset ->
            let cultureInfo = TextRuntime.GetCulture cultureStr
            fun json -> (JsonConversions2.AsDateTimeOffset cultureInfo json).IsSome
        | InferedTypeTag.TimeSpan ->
            JsonConversions2.AsTimeSpan(TextRuntime.GetCulture cultureStr)
            >> Option.isSome
        | InferedTypeTag.Guid -> JsonConversions2.AsGuid >> Option.isSome
        | InferedTypeTag.Collection ->
            function
            | JsonValue2.Array _ -> true
            | _ -> false
        | InferedTypeTag.Record _ ->
            function
            | JsonValue2.Record _ -> true
            | _ -> false
        | InferedTypeTag.Json -> failwith "Json type not supported"
        | InferedTypeTag.Null -> failwith "Null type not supported"
        | InferedTypeTag.Heterogeneous -> failwith "Heterogeneous type not supported"

    /// Returns all array values that match the specified tag
    static member GetArrayChildrenByTypeTag<'T>
        (
            doc: IJsonDocument2,
            cultureStr,
            tagCode,
            mapping: Func<IJsonDocument2, 'T>
        ) =
        match doc.JsonValue with
        | JsonValue2.Array elements ->
            elements
            |> Array.filter (JsonRuntime2.Matches cultureStr (InferedTypeTag.ParseCode tagCode))
            |> Array.mapi (fun i value ->
                doc.CreateNew(value, "[" + (string i) + "]")
                |> mapping.Invoke)
        | JsonValue2.Null -> [||]
        | x ->
            failwithf "Expecting an array at '%s', got %s" (doc.Path())
            <| x.ToString(JsonSaveOptions2.DisableFormatting)

    /// Returns single or no value from an array matching the specified tag
    static member TryGetArrayChildByTypeTag<'T>(doc, cultureStr, tagCode, mapping: Func<IJsonDocument2, 'T>) =
        match JsonRuntime2.GetArrayChildrenByTypeTag(doc, cultureStr, tagCode, mapping) with
        | [| child |] -> Some child
        | [||] -> None
        | _ ->
            failwithf "Expecting an array with single or no elements at '%s', got %s" (doc.Path())
            <| doc.JsonValue.ToString(JsonSaveOptions2.DisableFormatting)

    /// Returns a single array children that matches the specified tag
    static member GetArrayChildByTypeTag(doc, cultureStr, tagCode) =
        match JsonRuntime2.GetArrayChildrenByTypeTag(doc, cultureStr, tagCode, Func<_, _>(id)) with
        | [| child |] -> child
        | _ ->
            failwithf "Expecting an array with single element at '%s', got %s" (doc.Path())
            <| doc.JsonValue.ToString(JsonSaveOptions2.DisableFormatting)

    /// Returns a single or no value by tag type
    static member TryGetValueByTypeTag<'T>(doc: IJsonDocument2, cultureStr, tagCode, mapping: Func<IJsonDocument2, 'T>) =
        if JsonRuntime2.Matches cultureStr (InferedTypeTag.ParseCode tagCode) doc.JsonValue then
            Some(mapping.Invoke doc)
        else
            None

    static member private ToJsonValue2 (cultureInfo: CultureInfo) (value: obj) =
        let inline optionToJson f =
            function
            | None -> JsonValue2.Null
            | Some v -> f v

        match value with
        | null -> JsonValue2.Null
        | :? Array as v -> JsonValue2.Array [| for elem in v -> JsonRuntime2.ToJsonValue2 cultureInfo elem |]

        | :? string as v -> JsonValue2.String v
        | :? DateTime as v -> v.ToString("O", cultureInfo) |> JsonValue2.String
        | :? DateTimeOffset as v -> v.ToString("O", cultureInfo) |> JsonValue2.String
        | :? TimeSpan as v -> v.ToString("g", cultureInfo) |> JsonValue2.String
        | :? int as v -> JsonValue2.Number(decimal v)
        | :? int64 as v -> JsonValue2.Number(decimal v)
        | :? float as v -> JsonValue2.Number(decimal v)
        | :? decimal as v -> JsonValue2.Number v
        | :? bool as v -> JsonValue2.Boolean v
        | :? Guid as v -> v.ToString() |> JsonValue2.String
        | :? IJsonDocument2 as v -> v.JsonValue
        | :? JsonValue2 as v -> v

        | :? option<string> as v -> optionToJson JsonValue2.String v
        | :? option<DateTime> as v ->
            optionToJson (fun (dt: DateTime) -> dt.ToString(cultureInfo) |> JsonValue2.String) v
        | :? option<DateTimeOffset> as v ->
            optionToJson (fun (dt: DateTimeOffset) -> dt.ToString(cultureInfo) |> JsonValue2.String) v
        | :? option<TimeSpan> as v ->
            optionToJson (fun (ts: TimeSpan) -> ts.ToString("g", cultureInfo) |> JsonValue2.String) v
        | :? option<int> as v -> optionToJson (decimal >> JsonValue2.Number) v
        | :? option<int64> as v -> optionToJson (decimal >> JsonValue2.Number) v
        | :? option<float> as v -> optionToJson (decimal >> JsonValue2.Number) v
        | :? option<decimal> as v -> optionToJson JsonValue2.Number v
        | :? option<bool> as v -> optionToJson JsonValue2.Boolean v
        | :? option<Guid> as v -> optionToJson (fun (g: Guid) -> g.ToString() |> JsonValue2.String) v
        | :? option<IJsonDocument2> as v -> optionToJson (fun (v: IJsonDocument2) -> v.JsonValue) v
        | :? option<JsonValue2> as v -> optionToJson id v

        | _ -> failwithf "Can't create JsonValue2 from %A" value

    /// Creates a scalar JsonValue2 and wraps it in a json document
    static member CreateValue(value: obj, cultureStr) =
        let cultureInfo = TextRuntime.GetCulture cultureStr
        let json = JsonRuntime2.ToJsonValue2 cultureInfo value
        JsonDocument2.Create(json, "")

    // Creates a JsonValue2.Record and wraps it in a json document
    static member CreateRecord(properties, cultureStr) =
        let cultureInfo = TextRuntime.GetCulture cultureStr

        let json =
            properties
            |> Array.map (fun (k, v: obj) -> k, JsonRuntime2.ToJsonValue2 cultureInfo v)
            |> JsonValue2.Record

        JsonDocument2.Create(json, "")

    // Creates a JsonValue2.Record from key*value seq and wraps it in a json document
    static member CreateRecordFromDictionary<'Key, 'Value when 'Key: equality>
        (
            keyValuePairs: ('Key * 'Value) seq,
            cultureStr,
            mappingKeyBack: Func<'Key, string>
        ) =
        let cultureInfo = TextRuntime.GetCulture cultureStr

        let json =
            keyValuePairs
            |> Seq.map (fun (k, v) -> (k |> mappingKeyBack.Invoke), JsonRuntime2.ToJsonValue2 cultureInfo (v :> obj))
            |> Seq.toArray
            |> JsonValue2.Record

        JsonDocument2.Create(json, "")

    /// Creates a scalar JsonValue2.Array and wraps it in a json document
    /// elements is actually an obj[][]: an array of all the user-provided arrays from a ctor
    /// (e.g [ [1;2;3] ; ["a";"b";"c"]  ] in the case of an array inferred to contain IntsOrStrings)
    static member CreateArray(elements: obj[], cultureStr) =
        let cultureInfo = TextRuntime.GetCulture cultureStr

        let json =
            elements
            |> Array.collect (
                JsonRuntime2.ToJsonValue2 cultureInfo
                >> function
                    | JsonValue2.Array elements -> elements
                    | JsonValue2.Null -> [||]
                    | element -> [| element |]
            )
            |> JsonValue2.Array

        JsonDocument2.Create(json, "")
