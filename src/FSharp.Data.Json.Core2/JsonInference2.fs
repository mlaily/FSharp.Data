// --------------------------------------------------------------------------------------
// Implements type inference for JSON
// --------------------------------------------------------------------------------------

module ProviderImplementation.JsonInference2

open System
open FSharp.Data
open FSharp.Data.Runtime
open FSharp.Data.Runtime.StructuralTypes
open FSharp.Data.Runtime.StructuralInference
open System.Text.Json.Nodes
open System.Text.Json

/// Infer type of a JSON value - this is a simple function because most of the
/// functionality is handled in `StructureInference` (most notably, by
/// `inferCollectionType` and various functions to find common subtype), so
/// here we just need to infer types of primitive JSON values.
let rec internal inferType unitsOfMeasureProvider inferenceMode cultureInfo parentName (json: JsonNode) =
    let inline inRangeDecimal lo hi (v: decimal) : bool = (v >= decimal lo) && (v <= decimal hi)
    let inline inRangeFloat lo hi (v: float) : bool = (v >= float lo) && (v <= float hi)
    let inline isIntegerDecimal (v: decimal) : bool = Math.Round v = v
    let inline isIntegerFloat (v: float) : bool = Math.Round v = v

    let shouldInferNonStringFromValue =
        match inferenceMode with
        | InferenceMode'.NoInference -> false
        | InferenceMode'.ValuesOnly -> true
        | InferenceMode'.ValuesAndInlineSchemasHints -> true
        | InferenceMode'.ValuesAndInlineSchemasOverrides -> true

    match json with
    // Null and primitives without subtyping hierarchies
    | null -> InferedType.Null NullKind.NullToken

    | :? JsonObject as obj ->
        let name =
            if String.IsNullOrEmpty parentName then
                None
            else
                Some parentName

        let props =
            [ for KeyValue (propName, value) in obj ->
                  let t = inferType unitsOfMeasureProvider inferenceMode cultureInfo propName value
                  { Name = propName; Type = t } ]

        InferedType.Record(name, props, Mandatory)

    | :? JsonArray as ar ->
        StructuralInference.inferCollectionType
            false
            (Seq.map (inferType unitsOfMeasureProvider inferenceMode cultureInfo (NameUtils.singularize parentName)) ar)

    | :? Nodes.JsonValue as jValue ->
        match jValue.TryGetValue<JsonElement>() with
        | true, element ->
            match element.ValueKind with
            | JsonValueKind.True
            | JsonValueKind.False -> InferedType.Primitive(typeof<bool>, None, Mandatory, false, PrimitiveType.Bool)
            | JsonValueKind.String ->
                StructuralInference.inferPrimitiveType
                    unitsOfMeasureProvider
                    inferenceMode
                    cultureInfo
                    (element.GetString())
                    None
                    false
                    BooleanParsing.Strict
            | JsonValueKind.Number ->
                InferedType.Primitive(typeof<decimal>, None, Mandatory, false, PrimitiveType.Number)
            | other -> failwith $"Unexpected JsonValueKind: {other}"
        | false, other -> failwith $"Unexpected element (not a JsonElement): {other.GetType()}" // TODO (can only happen when json values have been manually created (not from parsing))

    | shouldNotBePossible ->
        failwith $"node implementation was of an unexpected type ({shouldNotBePossible.GetType()})."

//// For numbers, we test if it is integer and if it fits in smaller range
//| JsonValue2.Number n when
//    shouldInferNonStringFromValue
//    && inRangeDecimal Int32.MinValue Int32.MaxValue n
//    && isIntegerDecimal n
//    ->
//    InferedType.Primitive(typeof<int>, None, Mandatory, false, PrimitiveType.Number)
//| JsonValue2.Number n when
//    shouldInferNonStringFromValue
//    && inRangeDecimal Int64.MinValue Int64.MaxValue n
//    && isIntegerDecimal n
//    ->
//    InferedType.Primitive(typeof<int64>, None, Mandatory, false, PrimitiveType.Number)
//| JsonValue2.Number _ -> InferedType.Primitive(typeof<decimal>, None, Mandatory, false, PrimitiveType.Number)
//| JsonValue2.Float f when
//    shouldInferNonStringFromValue
//    && inRangeFloat Int32.MinValue Int32.MaxValue f
//    && isIntegerFloat f
//    ->
//    InferedType.Primitive(typeof<int>, None, Mandatory, false, PrimitiveType.Number)
//| JsonValue2.Float f when
//    shouldInferNonStringFromValue
//    && inRangeFloat Int64.MinValue Int64.MaxValue f
//    && isIntegerFloat f
//    ->
//    InferedType.Primitive(typeof<int64>, None, Mandatory, false, PrimitiveType.Number)
//| JsonValue2.Float _ -> InferedType.Primitive(typeof<float>, None, Mandatory, false, PrimitiveType.Number)
