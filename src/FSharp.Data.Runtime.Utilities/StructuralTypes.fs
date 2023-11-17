namespace rec FSharp.Data.Runtime.StructuralTypes

open System
open FSharp.Data.Runtime
open System.Diagnostics

// --------------------------------------------------------------------------------------
// Types that represent the result of the type inference
// --------------------------------------------------------------------------------------

/// <summary>A property of a record has a name and type and may be optional</summary>
/// <namespacedoc>
///   <summary>Types that represent the result of static type inference.</summary>
/// </namespacedoc>
[<Obsolete("This API will be made internal in a future release. Please file an issue at https://github.com/fsprojects/FSharp.Data/issues/1458 if you need this public.")>]
type InferedProperty =
    { Name: string
      mutable Type: InferedType }
    override x.ToString() = sprintf "%A" x

/// For heterogeneous types (types that have multiple possible forms
/// such as differently named XML nodes or records and arrays mixed together)
/// this type represents the number of occurrences of individual forms
[<Obsolete("This API will be made internal in a future release. Please file an issue at https://github.com/fsprojects/FSharp.Data/issues/1458 if you need this public.")>]
type InferedMultiplicity =
    | Single
    | OptionalSingle
    | Multiple

/// For heterogeneous types, this represents the tag that defines the form
/// (that is either primitive type, collection, named record etc.)
[<RequireQualifiedAccess>]
[<Obsolete("This API will be made internal in a future release. Please file an issue at https://github.com/fsprojects/FSharp.Data/issues/1458 if you need this public.")>]
type InferedTypeTag =
    // Unknown type
    | Null
    // Primitive types
    | Number
    | Boolean
    | String
    /// Allow for support of embedded json in e.g. xml documents
    | Json
    | DateTime
    | TimeSpan
    | DateTimeOffset
    | Guid
    // Collections and sum types
    | Collection
    | Heterogeneous
    // Possibly named record
    | Record of string option

/// Used to keep track of the original type of a primitive value (before inference).
/// This is primarily useful for the Json type provider,
/// to be able to serialize e.g an int inferred from a json string back into a json string instead of a json number.
[<RequireQualifiedAccess>]
[<Obsolete("This API will be made internal in a future release. Please file an issue at https://github.com/fsprojects/FSharp.Data/issues/1458 if you need this public.")>]
type PrimitiveType =
    | String
    | Number
    | Bool

    /// Serialization function useful to bypass type provider limitations...
    /// ("Quotations provided by type providers can only contain simple constants")
    static member ToInt(x) =
        match x with
        | None -> 0
        | Some String -> 1
        | Some Number -> 2
        | Some Bool -> 3

    /// Deserialization function useful to bypass type provider limitations...
    /// ("Quotations provided by type providers can only contain simple constants")
    static member FromInt(x) =
        match x with
        | 0 -> None
        | 1 -> Some String
        | 2 -> Some Number
        | 3 -> Some Bool
        | _ -> failwith $"PrimitiveType value {x} is not mapped."

// TODO: might be adapted for use with the xml provider: it doesn't have null tokens, but we currently cannot distinguish between an attribute with value "" from a missing attribute.
// (for types other than string, we are only able to generate missing attributes)
/// Used to keep track of why something was inferred as optional/null,
/// and be able to serialize back to the correct representation. (e.g. null token for the json provider)
[<RequireQualifiedAccess>]
[<Obsolete("This API will be made internal in a future release. Please file an issue at https://github.com/fsprojects/FSharp.Data/issues/1458 if you need this public.")>]
type NullKind =
    /// Something was inferred as null because the value was absent.
    | NoValue
    /// Something was inferred as null because the value was an explicit null token.
    | NullToken

    static member Merge(k1, k2) =
        match k1, k2 with
        | NullKind.NoValue, NullKind.NoValue -> NullKind.NoValue
        | NullKind.NullToken, _
        | _, NullKind.NullToken -> NullKind.NullToken // when we have a null token, it takes precedence

[<Obsolete("This API will be made internal in a future release. Please file an issue at https://github.com/fsprojects/FSharp.Data/issues/1458 if you need this public.")>]
[<DefaultAugmentation(false)>]
type InferedOptionality =
    | Optional of kind: NullKind
    | Mandatory

    member this.IsOptional =
        match this with
        | Optional _ -> true
        | Mandatory -> false

    static member FromBool(optional) =
        if optional then Optional NullKind.NoValue else Mandatory

    static member Merge(opt1, opt2) =
        match opt1, opt2 with
        | Mandatory, Mandatory -> Mandatory
        | Optional k, Mandatory
        | Mandatory, Optional k -> Optional k
        | Optional k1, Optional k2 -> Optional(NullKind.Merge(k1, k2))

[<Obsolete("This API will be made internal in a future release. Please file an issue at https://github.com/fsprojects/FSharp.Data/issues/1458 if you need this public.")>]
[<DefaultAugmentation(false)>]
type OptionalCollection =
    /// No `array option` will be generated, even if the collection was actually optional in the sample (e.g. sometimes null).
    /// This only makes sense for providers that don't have null tokens, but it is the backward compatible choice for the JsonProvider.
    | Disallow
    /// Allows generating `array option` types.
    | Allow

module internal InferedCollection =
    let inOrder order types =
        types
        |> Map.toList
        |> List.sortBy (fun (tag, _) -> List.findIndex ((=) tag) order)

/// Represents inferred structural type. A type may be either primitive type
/// (one of those listed by `primitiveTypes`) or it can be collection,
/// (named) record and heterogeneous type. We also have `Null` type (which is
/// a subtype of all non-primitive types) and universal `Top` type.
///
///  * For collection, we infer the types of different things that appear in
///    the collection and how many times they do.
///
///  * A heterogeneous type (sum type) is simply a choice containing one
///    of multiple different possibilities
///
/// Why is collection not simply a list of Heterogeneous types? If we used that
/// we would lose information about multiplicity and so we would not be able
/// to generate nicer types! (This is especially relevant for the XmlProvider)
[<CustomEquality; NoComparison; RequireQualifiedAccess>]
[<Obsolete("This API will be made internal in a future release. Please file an issue at https://github.com/fsprojects/FSharp.Data/issues/1458 if you need this public.")>]
[<DebuggerDisplay("{ToString(),nq}")>]
type InferedType =
    /// When shouldOverrideOnMerge is true, it means this type should win when merged with other primitive types during inference.
    /// This allows users to control inference by adding manual type hints that take priority.
    | Primitive of
        typ: Type *
        unit: option<System.Type> *
        optional: InferedOptionality *
        shouldOverrideOnMerge: bool *
        originalType: PrimitiveType
    | Record of name: string option * fields: InferedProperty list * optional: InferedOptionality
    | Json of typ: InferedType * optional: InferedOptionality
    | Collection of
        order: InferedTypeTag list *
        types: Map<InferedTypeTag, InferedMultiplicity * InferedType> *
        optional: InferedOptionality
    | Heterogeneous of types: Map<InferedTypeTag, InferedType> * containsOptional: InferedOptionality
    | Null of kind: NullKind
    | Top

    member this.GetOptionality() =
        match this with
        | Primitive (_, _, opt, _, _)
        | Record (_, _, opt)
        | Json (_, opt)
        | Collection (_, _, opt)
        | Heterogeneous (_, opt) -> opt
        | Null _
        | Top -> Mandatory

    /// Should the generated type be wrapped in an option?
    /// optionalCollections can be set to Disallow for backward compatibility to avoid ever generating an `array option`...
    member x.IsExplicitlyOptional optionalCollections =
        match x with
        // Heterogeneous are already intrinsically a list of options.
        | Heterogeneous _ -> false
        | Collection _ when optionalCollections = OptionalCollection.Disallow -> false
        | _ -> x.GetOptionality().IsOptional

    ///// Should the generated type be wrapped in an option?
    ///// optionalCollections can be set to Disallow for backward compatibility to avoid ever generating an `array option`...
    //member x.IsExplicitlyOptional optionalCollections =
    //    match x with
    //    // Heterogeneous are already intrinsically a list of options.
    //    | Heterogeneous (_, InferedOptionality.Optional NullKind.NoValue) -> // allows generating option of heterogeneous type (null can be handled at the heterogeneous type level, but missing value must be handled by parent)
    //    // TODO: should heterogeneous types explicit optionality be handled at the type level or at the parent level??. if we want to be able to generate null tokens in collections of heterogeneous types, it probably has to be at the type level.
    //    // but it also has to be at the parent level for missing values............
    //        match optionalCollections with // todo: change optionalcollection parameter to reflect the fact it can also impact heterogeneous types (or add another parameter...)
    //        | OptionalCollection.Allow -> true
    //        | OptionalCollection.Disallow -> false
    //    | Collection _ when optionalCollections = OptionalCollection.Disallow -> false
    //    | _ -> x.GetOptionality().IsOptional

    static member CanHaveEmptyValues typ =
        typ = typeof<string> || typ = typeof<float>

    /// When allowEmptyValues is true, we allow "" and double.NaN, otherwise
    /// we make the type optional and use None instead.
    /// It's currently only true in CsvProvider when PreferOptionals is set to false
    member x.EnsuresHandlesMissingValues allowEmptyValues nullKind =
        match x with
        | Primitive (typ, _, Mandatory, _, _) when
            allowEmptyValues
            && InferedType.CanHaveEmptyValues typ
            ->
            x
        | Null k -> Null(NullKind.Merge(k, nullKind))
        | Primitive (typ, unit, optional, overrideOnMerge, originalType) ->
            Primitive(typ, unit, InferedOptionality.Merge(optional, Optional nullKind), overrideOnMerge, originalType)
        | Record (name, props, optional) -> Record(name, props, InferedOptionality.Merge(optional, Optional nullKind))
        | Json (typ, optional) -> Json(typ, InferedOptionality.Merge(optional, Optional nullKind))
        | Heterogeneous (map, containsOptional) ->
            Heterogeneous(map, InferedOptionality.Merge(containsOptional, Optional nullKind))
        | Collection (order, types, optional) ->
            let typesR =
                types
                |> Map.map (fun _ (mult, typ) -> (if mult = Single then OptionalSingle else mult), typ)

            Collection(order, typesR, InferedOptionality.Merge(optional, Optional nullKind))
        | Top -> failwith "EnsuresHandlesMissingValues: unexpected InferedType.Top"

    member x.GetDropOptionality() =
        match x with
        | Primitive (typ, unit, Optional k, overrideOnMerge, originalType) ->
            Primitive(typ, unit, Mandatory, overrideOnMerge, originalType), Optional k
        | Record (name, props, Optional k) -> Record(name, props, Mandatory), Optional k
        | Json (typ, Optional k) -> Json(typ, Mandatory), Optional k
        | Heterogeneous (map, Optional k) -> Heterogeneous(map, Mandatory), Optional k
        | Collection (order, types, Optional k) -> Collection(order, types, Mandatory), Optional k
        | _ -> x, Mandatory

    member x.DropOptionality() = x.GetDropOptionality() |> fst

    // We need to implement custom equality that returns 'true' when
    // values reference the same object (to support recursive types)
    override x.GetHashCode() = -1

    override x.Equals(y: obj) =
        if y :? InferedType then
            match x, y :?> InferedType with
            | a, b when Object.ReferenceEquals(a, b) -> true
            | Primitive (t1, u1, opt1, x1, ot1), Primitive (t2, u2, opt2, x2, ot2) ->
                t1 = t2
                && u1 = u2
                && opt1 = opt2
                && x1 = x2
                && ot1 = ot2
            | Record (s1, pl1, opt1), Record (s2, pl2, opt2) -> s1 = s2 && pl1 = pl2 && opt1 = opt2
            | Json (t1, opt1), Json (t2, opt2) -> t1 = t2 && opt1 = opt2
            | Collection (o1, t1, opt1), Collection (o2, t2, opt2) -> o1 = o2 && t1 = t2 && opt1 = opt2
            | Heterogeneous (m1, opt1), Heterogeneous (m2, opt2) -> m1 = m2 && opt1 = opt2
            | Null k1, Null k2 -> k1 = k2
            | Top, Top -> true
            | _ -> false
        else
            false

    override x.ToString() =
        let sb = System.Text.StringBuilder()
        let mutable indentation = -1
        let pushIndent () = indentation <- indentation + 1
        let popIndent () = indentation <- indentation - 1

        let indented str =
            sb.Append(new String(' ', 2 * indentation))
            |> ignore

            sb.AppendLine(str) |> ignore

        let rec walk t =
            pushIndent ()

            match t with
            | Top -> indented ("(Top)") |> ignore
            | Null kind -> indented ($"(*Null* Kind: {kind})") |> ignore
            | Primitive (typ, unit, opt, overrideOnMerge, originalType) ->
                indented (
                    $"(*Primitive* %A{typ}, Unit: %A{unit}, Optional: {opt}, OverrideOnMerge: {overrideOnMerge}, OriginalType: {originalType})"
                )
                |> ignore
            | Record (name, props, opt) ->
                indented ($"{{*Record* Name: {name}, Optional: {opt}")
                |> ignore

                pushIndent ()

                for p in props do
                    indented ($"- Property (Name: {p.Name})")
                    |> ignore

                    walk p.Type

                popIndent ()
                indented ("}") |> ignore
            | Json (iTyp, opt) ->
                indented ($"{{Json Optional: {opt}") |> ignore
                walk iTyp
                indented ("}") |> ignore
            | Collection (order, types, optional) ->

                indented ($"[*Array* Optional: {optional}")
                |> ignore

                pushIndent ()

                for (typTag, (multiplicity, inferedType)) in InferedCollection.inOrder order types do
                    indented ($"- Item (Tag: {typTag}, Multiplicity: %A{multiplicity})")
                    |> ignore

                    walk inferedType

                popIndent ()
                indented ("]") |> ignore
            | Heterogeneous (inferedTypes, containsOptional) ->
                indented ($"{{[*Heterogeneous* ContainsOptional: {containsOptional}")
                |> ignore

                pushIndent ()

                for KeyValue (typTag, inferedType) in inferedTypes do
                    indented ($"- Type (Tag: {typTag})") |> ignore
                    walk inferedType

                popIndent ()
                indented ("]}") |> ignore

            popIndent ()

        walk x
        sb.ToString()

// ------------------------------------------------------------------------------------------------
// Additional operations for working with the inferred representation

type internal InferedTypeTag with
    member x.NiceName =
        match x with
        | Null -> failwith "Null nodes should be skipped"
        | Number -> "Number"
        | Boolean -> "Boolean"
        | String -> "String"
        | DateTime -> "DateTime"
        | TimeSpan -> "TimeSpan"
        | DateTimeOffset -> "DateTimeOffset"
        | Guid -> "Guid"
        | Collection -> "Array"
        | Heterogeneous -> "Choice"
        | Record None -> "Record"
        | Record (Some name) -> NameUtils.nicePascalName name
        | Json _ -> "Json"

    /// Converts tag to string code that can be passed to generated code
    member x.Code =
        match x with
        | Record (Some name) -> "Record@" + name
        | _ -> x.NiceName

    /// Parses code returned by 'Code' member (to be used in provided code)
    static member ParseCode(str: string) =
        match str with
        | s when s.StartsWith("Record@") -> Record(Some(s.Substring("Record@".Length)))
        | "Record" -> Record None
        | "Json" -> Json
        | "Number" -> Number
        | "Boolean" -> Boolean
        | "String" -> String
        | "DateTime" -> DateTime
        | "TimeSpan" -> TimeSpan
        | "DateTimeOffset" -> DateTimeOffset
        | "Guid" -> Guid
        | "Array" -> Collection
        | "Choice" -> Heterogeneous
        | "Null" -> failwith "Null nodes should be skipped"
        | _ -> failwith "Invalid InferredTypeTag code"

/// Dummy type to represent that only "0" was found.
/// Will be generated as 'int', unless it's converted to Bit.
[<Obsolete("This API will be made internal in a future release. Please file an issue at https://github.com/fsprojects/FSharp.Data/issues/1458 if you need this public.")>]
type Bit0 = Bit0

/// Dummy type to represent that only "1" was found
/// Will be generated as 'int', unless it's converted to Bit
[<Obsolete("This API will be made internal in a future release. Please file an issue at https://github.com/fsprojects/FSharp.Data/issues/1458 if you need this public.")>]
type Bit1 = Bit1

/// Dummy type to represent that only one of "0" and "1" were found
/// Will be generated as a 'bool', unless it's converted to another numerical type
[<Obsolete("This API will be made internal in a future release. Please file an issue at https://github.com/fsprojects/FSharp.Data/issues/1458 if you need this public.")>]
type Bit = Bit

// ------------------------------------------------------------------------------------------------

/// Represents a transformation of a type
[<RequireQualifiedAccess>]
[<Obsolete("This API will be made internal in a future release. Please file an issue at https://github.com/fsprojects/FSharp.Data/issues/1458 if you need this public.")>]
type TypeWrapper =
    /// No transformation will be made to the type
    | None
    /// The type T will be converter to type T option
    | Option
    /// The type T will be converter to type Nullable<T>
    | Nullable
    static member FromOption(optional: InferedOptionality) =
        match optional with
        | Optional _ -> TypeWrapper.Option
        | Mandatory -> TypeWrapper.None

/// Represents type information about a primitive value (used mainly in the CSV provider)
/// This type captures the type, unit of measure and handling of missing values (if we
/// infer that the value may be missing, we can generate option<T> or nullable<T>)
type internal PrimitiveInferedValue =
    { InferedType: Type
      RuntimeType: Type
      UnitOfMeasure: Type option
      TypeWrapper: TypeWrapper }

    static member Create(typ, typWrapper, unit) =
        let runtimeTyp =
            if typ = typeof<Bit> then
                typeof<bool>
            elif typ = typeof<Bit0> || typ = typeof<Bit1> then
                typeof<int>
            else
                typ

        { InferedType = typ
          RuntimeType = runtimeTyp
          UnitOfMeasure = unit
          TypeWrapper = typWrapper }

    static member Create(typ, optional, unit) =
        PrimitiveInferedValue.Create(typ, TypeWrapper.FromOption optional, unit)

/// Represents type information about a primitive property (used mainly in the CSV provider)
/// This type captures the type, unit of measure and handling of missing values (if we
/// infer that the value may be missing, we can generate option<T> or nullable<T>)
type internal PrimitiveInferedProperty =
    { Name: string
      Value: PrimitiveInferedValue }

    static member Create(name, typ, (typWrapper: TypeWrapper), unit) =
        { Name = name
          Value = PrimitiveInferedValue.Create(typ, typWrapper, unit) }

    static member Create(name, typ, optional, unit) =
        PrimitiveInferedProperty.Create(name, typ, TypeWrapper.FromOption optional, unit)
