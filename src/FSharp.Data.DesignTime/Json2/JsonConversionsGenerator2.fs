// ----------------------------------------------------------------------------------------------
// Conversions from string to various primitive types
// ----------------------------------------------------------------------------------------------

module ProviderImplementation.JsonConversionsGenerator2

open System
open FSharp.Quotations
open FSharp.Data
open FSharp.Data.Runtime
open FSharp.Data.Runtime.BaseTypes
open FSharp.Data.Runtime.StructuralTypes
open ProviderImplementation
open ProviderImplementation.QuotationBuilder

#nowarn "10001"

let getConversionQuotation cultureStr typ (value: Expr<JsonValue2 option>) =
    if typ = typeof<string> then
        <@@ JsonRuntime2.ConvertString(cultureStr, %value) @@>
    elif typ = typeof<int>
         || typ = typeof<Bit0>
         || typ = typeof<Bit1>
         || typ = typeof<Bit> then
        <@@ JsonRuntime2.ConvertInteger(cultureStr, %value) @@>
    elif typ = typeof<int64> then
        <@@ JsonRuntime2.ConvertInteger64(cultureStr, %value) @@>
    elif typ = typeof<decimal> then
        <@@ JsonRuntime2.ConvertDecimal(cultureStr, %value) @@>
    elif typ = typeof<float> then
        <@@ JsonRuntime2.ConvertFloat(cultureStr, %value) @@>
    elif typ = typeof<bool> then
        <@@ JsonRuntime2.ConvertBoolean(%value) @@>
    elif typ = typeof<DateTimeOffset> then
        <@@ JsonRuntime2.ConvertDateTimeOffset(cultureStr, %value) @@>
    elif typ = typeof<DateTime> then
        <@@ JsonRuntime2.ConvertDateTime(cultureStr, %value) @@>
    elif typ = typeof<TimeSpan> then
        <@@ JsonRuntime2.ConvertTimeSpan(cultureStr, %value) @@>
    elif typ = typeof<Guid> then
        <@@ JsonRuntime2.ConvertGuid(%value) @@>
    else
        failwith "getConversionQuotation: Unsupported primitive type"

type internal JsonConversionCallingType2 =
    | JsonDocument
    | JsonValueOption
    | JsonValueOptionAndPath

/// Creates a function that takes Expr<JsonValue option> and converts it to
/// an expression of other type - the type is specified by `field`
let internal convertJsonValue
    cultureStr
    canPassAllConversionCallingTypes
    (field: PrimitiveInferedValue)
    =

    let returnType =
        match field.TypeWrapper with
        | TypeWrapper.None -> field.TypeWithMeasure
        | TypeWrapper.Option -> typedefof<option<_>>.MakeGenericType field.TypeWithMeasure
        | TypeWrapper.Nullable -> typedefof<Nullable<_>>.MakeGenericType field.TypeWithMeasure

    let wrapInLetIfNeeded (value: Expr) getBody =
        match value with
        | Patterns.Var var ->
            let varExpr = Expr.Cast<'T>(Expr.Var var)
            getBody varExpr
        | _ ->
            let var = Var("value", typeof<'T>)
            let varExpr = Expr.Cast<'T>(Expr.Var var)
            Expr.Let(var, value, getBody varExpr)

    let convert (value: Expr) =
        let convert value =
            getConversionQuotation cultureStr field.InferedType value

        match field.TypeWrapper, canPassAllConversionCallingTypes with
        | TypeWrapper.None, true ->
            wrapInLetIfNeeded value (fun (varExpr: Expr<JsonValueOptionAndPath2>) ->
                typeof<JsonRuntime2>?(nameof (JsonRuntime2.GetNonOptionalValue))
                    (field.RuntimeType)
                    (<@ (%varExpr).Path @>, convert <@ (%varExpr).JsonOpt @>, <@ (%varExpr).JsonOpt @>))
        | TypeWrapper.None, false ->
            wrapInLetIfNeeded value (fun (varExpr: Expr<IJsonDocument2>) ->
                typeof<JsonRuntime2>?(nameof (JsonRuntime2.GetNonOptionalValue))
                    (field.RuntimeType)
                    (<@ (%varExpr).Path() @>, convert <@ Some (%varExpr).JsonValue @>, <@ Some (%varExpr).JsonValue @>))
        | TypeWrapper.Option, true -> convert <@ (%%value: JsonValue2 option) @>
        | TypeWrapper.Option, false ->
            //TODO: not covered in tests
            convert <@ Some (%%value: IJsonDocument2).JsonValue @>
        | TypeWrapper.Nullable, true ->
            //TODO: not covered in tests
            typeof<TextRuntime>?(nameof (TextRuntime.OptionToNullable))
                (field.RuntimeType)
                (convert <@ (%%value: JsonValue2 option) @>)
        | TypeWrapper.Nullable, false ->
            //TODO: not covered in tests
            typeof<TextRuntime>?(nameof (TextRuntime.OptionToNullable))
                (field.RuntimeType)
                (convert <@ Some (%%value: IJsonDocument2).JsonValue @>)

    let conversionCallingType =
        if canPassAllConversionCallingTypes then
            match field.TypeWrapper with
            | TypeWrapper.None -> JsonValueOptionAndPath
            | TypeWrapper.Option
            | TypeWrapper.Nullable -> JsonValueOption
        else
            JsonDocument

    returnType, convert, conversionCallingType
