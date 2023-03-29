// --------------------------------------------------------------------------------------
// Helper operations for converting converting json values to other types
// --------------------------------------------------------------------------------------

namespace FSharp.Data.Runtime

open System
open FSharp.Data

[<AutoOpen>]
module private Helpers =
    let inline inRangeDecimal lo hi (v: decimal) : bool = (v >= decimal lo) && (v <= decimal hi)
    let inline inRangeFloat lo hi (v: float) : bool = (v >= float lo) && (v <= float hi)
    let inline isIntegerDecimal (v: decimal) : bool = Math.Round v = v
    let inline isIntegerFloat (v: float) : bool = Math.Round v = v

/// Conversions from JsonValue to string/int/int64/decimal/float/boolean/datetime/datetimeoffset/timespan/guid options
type JsonConversions2 =

    static member AsString useNoneForNullOrEmpty (cultureInfo: IFormatProvider) =
        function
        | JsonValue2.String s ->
            if useNoneForNullOrEmpty && String.IsNullOrEmpty s then
                None
            else
                Some s
        | JsonValue2.Boolean b -> Some(if b then "true" else "false")
        | JsonValue2.Number n -> n.ToString(cultureInfo) |> Some
        | JsonValue2.Float f -> f.ToString(cultureInfo) |> Some
        | JsonValue2.Null when not useNoneForNullOrEmpty -> Some ""
        | _ -> None

    static member AsInteger cultureInfo =
        function
        | JsonValue2.Number n when
            inRangeDecimal Int32.MinValue Int32.MaxValue n
            && isIntegerDecimal n
            ->
            Some(int n)
        | JsonValue2.Float f when
            inRangeFloat Int32.MinValue Int32.MaxValue f
            && isIntegerFloat f
            ->
            Some(int f)
        | JsonValue2.String s -> TextConversions.AsInteger cultureInfo s
        | _ -> None

    static member AsInteger64 cultureInfo =
        function
        | JsonValue2.Number n when
            inRangeDecimal Int64.MinValue Int64.MaxValue n
            && isIntegerDecimal n
            ->
            Some(int64 n)
        | JsonValue2.Float f when
            inRangeFloat Int64.MinValue Int64.MaxValue f
            && isIntegerFloat f
            ->
            Some(int64 f)
        | JsonValue2.String s -> TextConversions.AsInteger64 cultureInfo s
        | _ -> None

    static member AsDecimal cultureInfo =
        function
        | JsonValue2.Number n -> Some n
        | JsonValue2.String s -> TextConversions.AsDecimal cultureInfo s
        | _ -> None

    static member AsFloat missingValues useNoneForMissingValues cultureInfo =
        function
        | JsonValue2.Number n -> Some(float n)
        | JsonValue2.Float n -> Some n
        | JsonValue2.String s -> TextConversions.AsFloat missingValues useNoneForMissingValues cultureInfo s
        | _ -> None

    static member AsBoolean =
        function
        | JsonValue2.Boolean b -> Some b
        | JsonValue2.Number 1M -> Some true
        | JsonValue2.Number 0M -> Some false
        | JsonValue2.String s -> TextConversions.AsBoolean BooleanParsing.Strict s
        | _ -> None

    static member AsDateTimeOffset cultureInfo =
        function
        | JsonValue2.String s -> TextConversions.AsDateTimeOffset cultureInfo s
        | _ -> None

    static member AsDateTime cultureInfo =
        function
        | JsonValue2.String s -> TextConversions.AsDateTime cultureInfo s
        | _ -> None

    static member AsTimeSpan cultureInfo =
        function
        | JsonValue2.String s -> TextConversions.AsTimeSpan cultureInfo s
        | _ -> None

    static member AsGuid =
        function
        | JsonValue2.String s -> TextConversions.AsGuid s
        | _ -> None
