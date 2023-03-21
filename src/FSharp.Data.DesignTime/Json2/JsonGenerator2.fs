// --------------------------------------------------------------------------------------
// JSON type provider - generate code for accessing inferred elements
// --------------------------------------------------------------------------------------
namespace ProviderImplementation

open System
open System.Collections.Generic
open FSharp.Quotations
open FSharp.Data
open FSharp.Data.Runtime
open FSharp.Data.Runtime.BaseTypes
open FSharp.Data.Runtime.StructuralTypes
open ProviderImplementation
open ProviderImplementation.JsonConversionsGenerator2
open ProviderImplementation.ProvidedTypes
open FSharp.Data.Runtime.StructuralInference

#nowarn "10001"

/// Context that is used to generate the JSON types.
type internal JsonGenerationContext2 =
    { CultureStr: string
      TypeProviderType: ProvidedTypeDefinition
      // to nameclash type names
      UniqueNiceName: string -> string
      IJsonDocumentType: Type
      JsonValueType: Type
      JsonRuntimeType: Type
      TypeCache: Dictionary<InferedType, ProvidedTypeDefinition>
      PreferDictionaries: bool
      GenerateConstructors: bool
      InferenceMode: InferenceMode'
      UnitsOfMeasureProvider: IUnitsOfMeasureProvider }

    static member Create
        (
            cultureStr,
            tpType,
            unitsOfMeasureProvider,
            inferenceMode,
            ?uniqueNiceName,
            ?typeCache,
            ?preferDictionaries
        ) =
        let uniqueNiceName =
            defaultArg uniqueNiceName (NameUtils.uniqueGenerator NameUtils.nicePascalName)

        let typeCache = defaultArg typeCache (Dictionary())
        let preferDictionaries = defaultArg preferDictionaries false

        JsonGenerationContext2.Create(
            cultureStr,
            tpType,
            uniqueNiceName,
            typeCache,
            preferDictionaries,
            true,
            inferenceMode,
            unitsOfMeasureProvider
        )

    static member Create
        (
            cultureStr,
            tpType,
            uniqueNiceName,
            typeCache,
            preferDictionaries,
            generateConstructors,
            inferenceMode,
            unitsOfMeasureProvider
        ) =
        { CultureStr = cultureStr
          TypeProviderType = tpType
          UniqueNiceName = uniqueNiceName
          IJsonDocumentType = typeof<IJsonDocument2>
          JsonValueType = typeof<JsonValue2>
          JsonRuntimeType = typeof<JsonRuntime2>
          TypeCache = typeCache
          PreferDictionaries = preferDictionaries
          GenerateConstructors = generateConstructors
          InferenceMode = inferenceMode
          UnitsOfMeasureProvider = unitsOfMeasureProvider }

    member x.MakeOptionType(typ: Type) =
        typedefof<option<_>>.MakeGenericType typ

type internal JsonGenerationResult2 =
    { ConvertedType: Type
      OptionalConverter: (Expr -> Expr) option
      ConversionCallingType: JsonConversionCallingType2 }

    member x.Convert = defaultArg x.OptionalConverter id

    member x.ConverterFunc ctx =
        ReflectionHelpers.makeDelegate x.Convert ctx.IJsonDocumentType

    member x.ConvertedTypeErased ctx =
        if x.ConvertedType.IsArray then
            match x.ConvertedType.GetElementType() with
            | :? ProvidedTypeDefinition -> ctx.IJsonDocumentType.MakeArrayType()
            | _ -> x.ConvertedType
        else
            match x.ConvertedType with
            | :? ProvidedTypeDefinition -> ctx.IJsonDocumentType
            | _ -> x.ConvertedType

module JsonTypeBuilder2 =

    let (?) = QuotationBuilder.(?)

    module internal List =
        let unzip4 l =
            let a, b, cd = List.unzip3 (List.map (fun (a, b, c, d) -> (a, b, (c, d))) l)
            let c, d = List.unzip cd
            a, b, c, d

    let internal findOriginalPrimitiveType inferedType =
        let (|SingleTypeCollection|_|) =
            function
            | InferedType.Collection ([ singleTag ], types) ->
                let _, singleType = types[singleTag]
                Some singleType
            | _ -> None

        match inferedType with
        | InferedType.Primitive (_, _, _, _, originalType) -> Some originalType
        | SingleTypeCollection (InferedType.Primitive (_, _, _, _, originalType)) -> Some originalType
        | _ -> None

    // check if a type was already created for the inferedType before creating a new one
    let internal getOrCreateType ctx inferedType createType =

        // normalize properties of the inferedType which don't affect code generation
        let rec normalize topLevel =
            function
            | InferedType.Heterogeneous (map, _) ->
                map
                |> Map.map (fun _ inferedType -> normalize false inferedType)
                |> (fun x -> InferedType.Heterogeneous(x, Mandatory))
            | InferedType.Collection (order, types) ->
                InferedType.Collection(
                    order,
                    Map.map (fun _ (multiplicity, inferedType) -> multiplicity, normalize false inferedType) types
                )
            | InferedType.Record (_, props, optional) ->
                let props =
                    props
                    |> List.map (fun { Name = name; Type = inferedType } ->
                        { Name = name
                          Type = normalize false inferedType })
                // optional only affects the parent, so at top level always set to optional regardless of the actual value
                InferedType.Record(None, props, InferedOptionality.Merge(optional, InferedOptionality.FromBool(topLevel)))
            | InferedType.Primitive (typ, unit, optional, shouldOverrideOnMerge, originalType) when
                typ = typeof<Bit0> || typ = typeof<Bit1> || typ = typeof<Bit>
                ->
                InferedType.Primitive(typeof<int>, unit, optional, shouldOverrideOnMerge, originalType)
            | x -> x

        let inferedType = normalize true inferedType

        let typ =
            match ctx.TypeCache.TryGetValue inferedType with
            | true, typ -> typ
            | _ ->
                let typ = createType ()
                ctx.TypeCache.Add(inferedType, typ)
                typ

        { ConvertedType = typ
          OptionalConverter = None
          ConversionCallingType = JsonDocument }

    let internal replaceJDocWithJValue (ctx: JsonGenerationContext2) (typ: Type) =
        if typ = ctx.IJsonDocumentType then
            ctx.JsonValueType
        elif typ.IsArray
             && typ.GetElementType() = ctx.IJsonDocumentType then
            ctx.JsonValueType.MakeArrayType()
        elif typ.IsGenericType
             && typ.GetGenericArguments() = [| ctx.IJsonDocumentType |] then
            typ.GetGenericTypeDefinition().MakeGenericType ctx.JsonValueType
        else
            typ

    /// Common code that is shared by code generators that generate
    /// "Choice" type. This is parameterized by the types (choices) to generate,
    /// by functions that get the multiplicity and the type tag for each option
    /// and also by function that generates the actual code.
    let rec internal generateMultipleChoiceType
        ctx
        types
        forCollection
        nameOverride
        (codeGenerator: _ -> _ -> _ -> Expr)
        =

        let types =
            types
            |> Seq.map (fun (KeyValue (tag, (multiplicity, inferedType))) -> tag, multiplicity, inferedType)
            |> Seq.sortBy (fun (tag, _, _) -> tag)
            |> Seq.toArray

        if types.Length <= 1 then
            failwithf "generateMultipleChoiceType: Invalid choice type: %A" types

        for _, _, inferedType in types do
            match inferedType with
            | InferedType.Null _
            | InferedType.Top
            | InferedType.Heterogeneous _ -> failwithf "generateMultipleChoiceType: Unsupported type: %A" inferedType
            | x when x.IsOptional -> failwithf "generateMultipleChoiceType: Type shouldn't be optional: %A" inferedType
            | _ -> ()

        let typeName =
            if not (String.IsNullOrEmpty nameOverride) then
                nameOverride
            else
                let getTypeName (tag: InferedTypeTag, multiplicity, inferedType) =
                    match multiplicity with
                    | InferedMultiplicity.Single -> failwith "Single multiplicity not supported"
                    | InferedMultiplicity.Multiple -> NameUtils.pluralize tag.NiceName
                    | InferedMultiplicity.OptionalSingle ->
                        match inferedType with
                        | InferedType.Primitive (typ, _, _, _, _) ->
                            if typ = typeof<int>
                               || typ = typeof<Bit0>
                               || typ = typeof<Bit1> then
                                "Int"
                            elif typ = typeof<int64> then
                                "Int64"
                            elif typ = typeof<decimal> then
                                "Decimal"
                            elif typ = typeof<float> then
                                "Float"
                            else
                                tag.NiceName
                        | _ -> tag.NiceName

                types
                |> Array.map getTypeName
                |> String.concat "Or"
            |> ctx.UniqueNiceName

        // Generate new type for the heterogeneous type
        let objectTy =
            ProvidedTypeDefinition(typeName, Some ctx.IJsonDocumentType, hideObjectMethods = true, nonNullable = true)

        ctx.TypeProviderType.AddMember objectTy

        // to nameclash property names
        let makeUnique = NameUtils.uniqueGenerator NameUtils.nicePascalName
        makeUnique "JsonValue" |> ignore

        let members =
            [ for tag, multiplicity, inferedType in types ->

                  let result = generateJsonType ctx false false "" inferedType

                  let propName =
                      match tag with
                      | InferedTypeTag.Record _ -> "Record"
                      | _ -> tag.NiceName

                  let name, typ, constructorType =
                      match multiplicity with
                      | InferedMultiplicity.Single -> failwith "Single multiplicity not supported"
                      | InferedMultiplicity.OptionalSingle ->
                          assert (forCollection = false)
                          makeUnique propName,
                          ctx.MakeOptionType result.ConvertedType,
                          replaceJDocWithJValue ctx result.ConvertedType
                      | InferedMultiplicity.Multiple ->
                          makeUnique (NameUtils.pluralize tag.NiceName),
                          result.ConvertedType.MakeArrayType(),
                          (replaceJDocWithJValue ctx result.ConvertedType).MakeArrayType()

                  ProvidedProperty(name, typ, getterCode = codeGenerator result tag.Code),
                  ProvidedParameter(NameUtils.niceCamelName name, constructorType),
                  findOriginalPrimitiveType inferedType ]

        let properties, parameters, originalPrimitiveTypes = List.unzip3 members
        objectTy.AddMembers properties

        if ctx.GenerateConstructors then

            let cultureStr = ctx.CultureStr

            if forCollection then
                let ctorCode (args: Expr list) =
                    let elements =
                        Expr.NewArray(
                            typeof<obj * int>,
                            args
                            |> List.mapi (fun i a ->
                                let serializedOriginalPrimitiveType =
                                    originalPrimitiveTypes[i] |> PrimitiveType.ToInt

                                let arg = Expr.Coerce(a, typeof<obj>)
                                <@@ (%%arg, serializedOriginalPrimitiveType) @@>)
                        )

                    let cultureStr = ctx.CultureStr
                    <@@ JsonRuntime2.CreateArray(%%elements, cultureStr) @@>

                let ctor = ProvidedConstructor(parameters, invokeCode = ctorCode)
                objectTy.AddMember ctor
            else
                for param in parameters do
                    let ctorCode (Singleton arg: Expr list) =
                        let arg = Expr.Coerce(arg, typeof<obj>)
                        <@@ JsonRuntime2.CreateValue((%%arg: obj), cultureStr) @@>

                    let ctor = ProvidedConstructor([ param ], invokeCode = ctorCode)
                    objectTy.AddMember ctor

                let defaultCtor =
                    let ctorCode _ =
                        <@@ JsonRuntime2.CreateValue(null :> obj, cultureStr) @@>

                    ProvidedConstructor([], invokeCode = ctorCode)

                objectTy.AddMember defaultCtor

            let ctorCode (Singleton arg) =
                <@@ JsonDocument2.Create((%%arg: JsonValue2), "") @@>

            let ctor =
                ProvidedConstructor([ ProvidedParameter("jsonValue", ctx.JsonValueType) ], invokeCode = ctorCode)

            objectTy.AddMember ctor

        objectTy

    /// Recursively walks over inferred type information and
    /// generates types for read-only access to the document
    and internal generateJsonType
        ctx
        canPassAllConversionCallingTypes
        optionalityHandledByParent
        nameOverride
        inferedType
        =

        let inferedType =
            match inferedType with
            | InferedType.Collection (order, types) ->
                InferedType.Collection(
                    List.filter ((<>) InferedTypeTag.Null) order,
                    Map.remove InferedTypeTag.Null types
                    // Ignore inferred collection multiplicity since json has an unambiguous collection representation.
                    |> Map.map (fun _ (_, typ) -> InferedMultiplicity.Multiple, typ)
                )
            | x -> x

        match inferedType with

        | InferedType.Primitive (inferedType, unit, optional, _, _) ->

            let typ, conv, conversionCallingType =
                PrimitiveInferedValue.Create(inferedType, optional, unit)
                |> convertJsonValue "" ctx.CultureStr canPassAllConversionCallingTypes

            { ConvertedType = typ
              OptionalConverter = Some conv
              ConversionCallingType = conversionCallingType }

        | InferedType.Top
        | InferedType.Null _ ->

            // Return the underlying JsonDocument without change
            { ConvertedType = ctx.IJsonDocumentType
              OptionalConverter = None
              ConversionCallingType = JsonDocument }

        | InferedType.Collection (_, SingletonMap (_, (_, typ)))
        | InferedType.Collection (_, EmptyMap InferedType.Top typ) ->

            let elementResult = generateJsonType ctx false false nameOverride typ

            let conv =
                fun (jDoc: Expr) ->
                    ctx.JsonRuntimeType?(nameof (JsonRuntime2.ConvertArray))
                        (elementResult.ConvertedTypeErased ctx)
                        (jDoc, elementResult.ConverterFunc ctx)

            { ConvertedType = elementResult.ConvertedType.MakeArrayType()
              OptionalConverter = Some conv
              ConversionCallingType = JsonDocument }

        | InferedType.Record (name, props, optional) ->
            getOrCreateType ctx inferedType (fun () ->

                if optional.IsOptional && not optionalityHandledByParent then
                    failwithf "generateJsonType: optionality not handled for %A" inferedType

                let name =
                    if String.IsNullOrEmpty nameOverride then
                        match name with
                        | Some name -> name
                        | _ -> "Record"
                    else
                        nameOverride
                    |> ctx.UniqueNiceName

                // Generate new type for the record
                let objectTy =
                    ProvidedTypeDefinition(
                        name,
                        Some ctx.IJsonDocumentType,
                        hideObjectMethods = true,
                        nonNullable = true
                    )

                ctx.TypeProviderType.AddMember(objectTy)

                // to nameclash property names
                let makeUnique = NameUtils.uniqueGenerator NameUtils.nicePascalName
                makeUnique "JsonValue" |> ignore

                let inferedKeyValueType =

                    let aggr = List.fold (StructuralInference.subtypeInfered false) InferedType.Top

                    let dropRecordsNames infType =

                        let dropRecordName infType =
                            match infType with
                            | InferedType.Record (_, fields, opt) -> InferedType.Record(None, fields, opt)
                            | _ -> infType

                        let dropTagName tag =
                            match tag with
                            | InferedTypeTag.Record (Some _) -> InferedTypeTag.Record None
                            | _ -> tag

                        let infType = dropRecordName infType

                        match infType with
                        | InferedType.Collection (order, types) ->
                            // Records in collections have the parent property as name.
                            // We drop it too so they can be merged into a unified type.
                            let order = order |> List.map dropTagName

                            let types =
                                types
                                |> Map.toSeq
                                |> Seq.map (fun (tag, (multiplicity, typ)) ->
                                    let tag = dropTagName tag
                                    let typ = dropRecordName typ
                                    tag, (multiplicity, typ))
                                |> Map.ofSeq

                            InferedType.Collection(order, types)
                        | _ -> infType

                    if not ctx.PreferDictionaries then
                        None
                    else
                        let infKeyType =
                            [ for prop in props ->
                                  StructuralInference.inferPrimitiveType
                                      ctx.UnitsOfMeasureProvider
                                      ctx.InferenceMode
                                      (TextRuntime.GetCulture ctx.CultureStr)
                                      prop.Name
                                      None
                                      false
                                      BooleanParsing.Strict ]
                            |> aggr

                        match infKeyType with
                        | InferedType.Primitive (typ = typ) when typ <> typeof<string> ->
                            let inferValueType =
                                ([ for prop in props -> prop.Type |> dropRecordsNames ]
                                 |> aggr)
                                    // Optional properties in the initial record should translate
                                    // to simply missing values in the dictionary, not an optional type.
                                    .DropOptionality()

                            Some(infKeyType, inferValueType)
                        | _ -> None

                match inferedKeyValueType with
                | Some (inferedKeyType, inferedValueType) ->
                    // Add all record fields as dictionary items
                    let valueName = name + "Value"

                    let keyResult = generateJsonType ctx false true "" inferedKeyType
                    let valueResult = generateJsonType ctx false true valueName inferedValueType
                    let valueConvertedTypeErased = valueResult.ConvertedTypeErased ctx

                    let tupleType =
                        Microsoft.FSharp.Reflection.FSharpType.MakeTupleType(
                            [| keyResult.ConvertedType; valueResult.ConvertedType |]
                        )

                    let itemsSeqType = typedefof<_ seq>.MakeGenericType ([| tupleType |])

                    let itemsGetter (Singleton jDoc) =
                        ctx.JsonRuntimeType?(nameof (JsonRuntime2.ConvertRecordToDictionary))
                            (keyResult.ConvertedType, valueConvertedTypeErased)
                            (jDoc, keyResult.ConverterFunc ctx, valueResult.ConverterFunc ctx)

                    let keysGetter (Singleton jDoc) =
                        ctx.JsonRuntimeType?(nameof (JsonRuntime2.GetKeysFromInferedDictionary))
                            (keyResult.ConvertedType)
                            (jDoc, keyResult.ConverterFunc ctx)

                    let valuesGetter (Singleton jDoc) =
                        ctx.JsonRuntimeType?(nameof (JsonRuntime2.GetValuesFromInferedDictionary))
                            (valueConvertedTypeErased)
                            (jDoc, valueResult.ConverterFunc ctx)

                    let (|Doubleton|) =
                        function
                        | [ f; s ] -> f, s
                        | _ -> failwith "Parameter mismatch"

                    let itemGetter (Doubleton (jDoc, key)) =
                        ctx.JsonRuntimeType?(nameof (JsonRuntime2.GetValueByKeyFromInferedDictionary))
                            (keyResult.ConvertedType, valueConvertedTypeErased)
                            (jDoc, keyResult.ConverterFunc ctx, valueResult.ConverterFunc ctx, key)

                    let tryFindCode (Doubleton (jDoc, key)) =
                        ctx.JsonRuntimeType?(nameof (JsonRuntime2.TryGetValueByKeyFromInferedDictionary))
                            (keyResult.ConvertedType, valueConvertedTypeErased)
                            (jDoc, keyResult.ConverterFunc ctx, valueResult.ConverterFunc ctx, key)

                    let containsKeyCode (Doubleton (jDoc, key)) =
                        ctx.JsonRuntimeType?(nameof (JsonRuntime2.InferedDictionaryContainsKey))
                            (keyResult.ConvertedType)
                            (jDoc, keyResult.ConverterFunc ctx, key)

                    let countGetter (Singleton jDoc) =
                        <@@ JsonRuntime2.GetRecordProperties(%%jDoc).Length @@>

                    let isEmptyGetter (Singleton jDoc) =
                        <@@ JsonRuntime2.GetRecordProperties(%%jDoc).Length = 0 @@>

                    [ ProvidedProperty("Items", itemsSeqType, getterCode = itemsGetter)
                      ProvidedProperty("Keys", keyResult.ConvertedType.MakeArrayType(), getterCode = keysGetter)
                      ProvidedProperty("Values", valueResult.ConvertedType.MakeArrayType(), getterCode = valuesGetter)
                      ProvidedProperty(
                          "Item",
                          valueResult.ConvertedType,
                          getterCode = itemGetter,
                          indexParameters = [ ProvidedParameter("key", keyResult.ConvertedType) ]
                      )
                      ProvidedProperty("Count", typeof<int>, getterCode = countGetter)
                      ProvidedProperty("IsEmpty", typeof<bool>, getterCode = isEmptyGetter) ]
                    |> objectTy.AddMembers

                    [ ProvidedMethod(
                          "TryFind",
                          [ ProvidedParameter("key", keyResult.ConvertedType) ],
                          valueResult.ConvertedType |> ctx.MakeOptionType,
                          tryFindCode
                      )
                      ProvidedMethod(
                          "ContainsKey",
                          [ ProvidedParameter("key", keyResult.ConvertedType) ],
                          typeof<bool>,
                          containsKeyCode
                      ) ]
                    |> objectTy.AddMembers

                    if ctx.GenerateConstructors then
                        let conv (value: Expr) =
                            let value = ProviderHelpers.some keyResult.ConvertedType value

                            ConversionsGenerator.getBackConversionQuotation
                                ""
                                ctx.CultureStr
                                keyResult.ConvertedType
                                value
                            :> Expr

                        let ctorCode (args: Expr list) =
                            let kvSeq = args.Head
                            let convFunc = ReflectionHelpers.makeDelegate conv keyResult.ConvertedType
                            let cultureStr = ctx.CultureStr

                            let originalValueType =
                                findOriginalPrimitiveType inferedValueType
                                |> PrimitiveType.ToInt
                                |> Expr.Value

                            ctx.JsonRuntimeType?(nameof (JsonRuntime2.CreateRecordFromDictionary))
                                (keyResult.ConvertedType, valueConvertedTypeErased)
                                (kvSeq, cultureStr, convFunc, originalValueType)

                        let ctor =
                            ProvidedConstructor([ ProvidedParameter("items", itemsSeqType) ], ctorCode)

                        objectTy.AddMember ctor

                    ()
                | None ->
                    // Add all record fields as properties
                    let members =
                        [ for prop in props ->

                              let propResult = generateJsonType ctx true true "" prop.Type
                              let propName = prop.Name
                              let optionalityHandledByProperty = propResult.ConversionCallingType <> JsonDocument

                              let getter (Singleton jDoc) =

                                  if optionalityHandledByProperty then

                                      propResult.Convert
                                      <| if propResult.ConversionCallingType = JsonValueOptionAndPath then
                                             <@@ JsonRuntime2.TryGetPropertyUnpackedWithPath(%%jDoc, propName) @@>
                                         else
                                             <@@ JsonRuntime2.TryGetPropertyUnpacked(%%jDoc, propName) @@>

                                  elif prop.Type.IsOptional then

                                      match propResult.OptionalConverter with
                                      | Some _ ->
                                          //TODO: not covered in tests
                                          ctx.JsonRuntimeType?(nameof (JsonRuntime2.ConvertOptionalProperty))
                                              (propResult.ConvertedTypeErased ctx)
                                              (jDoc, propName, propResult.ConverterFunc ctx)

                                      | None -> <@@ JsonRuntime2.TryGetPropertyPacked(%%jDoc, propName) @@>

                                  else

                                      propResult.Convert
                                      <| match prop.Type with
                                         | InferedType.Collection _
                                         | InferedType.Heterogeneous _
                                         | InferedType.Top
                                         | InferedType.Null _ ->
                                             <@@ JsonRuntime2.GetPropertyPackedOrNull(%%jDoc, propName) @@>
                                         | _ -> <@@ JsonRuntime2.GetPropertyPacked(%%jDoc, propName) @@>

                              let convertedType =
                                  if prop.Type.IsOptional
                                     && not optionalityHandledByProperty then
                                      ctx.MakeOptionType propResult.ConvertedType
                                  else
                                      propResult.ConvertedType

                              let name = makeUnique prop.Name

                              prop.Name,
                              ProvidedProperty(name, convertedType, getterCode = getter),
                              ProvidedParameter(NameUtils.niceCamelName name, replaceJDocWithJValue ctx convertedType),
                              findOriginalPrimitiveType prop.Type ]

                    let names, properties, parameters, originalPrimitiveTypes = List.unzip4 members

                    objectTy.AddMembers properties

                    if ctx.GenerateConstructors then
                        let ctorCode (args: Expr list) =
                            let properties =
                                Expr.NewArray(
                                    typeof<string * obj * int>,
                                    args
                                    |> List.mapi (fun i a ->
                                        let name = names[i]

                                        let serializedOriginalPrimitiveType =
                                            originalPrimitiveTypes[i] |> PrimitiveType.ToInt

                                        let arg = Expr.Coerce(a, typeof<obj>)
                                        <@@ (name, %%arg, serializedOriginalPrimitiveType) @@>)
                                )

                            let cultureStr = ctx.CultureStr

                            <@@ JsonRuntime2.CreateRecord(%%properties, cultureStr) @@>

                        let ctor = ProvidedConstructor(parameters, invokeCode = ctorCode)
                        objectTy.AddMember ctor

                    ()

                if ctx.GenerateConstructors then
                    let ctorCode (Singleton arg: Expr list) =
                        <@@ JsonDocument2.Create((%%arg: JsonValue2), "") @@>

                    let ctorParams = [ ProvidedParameter("jsonValue", ctx.JsonValueType) ]
                    let ctor = ProvidedConstructor(ctorParams, ctorCode)
                    objectTy.AddMember ctor

                objectTy)

        | InferedType.Collection (_, types) ->
            getOrCreateType ctx inferedType (fun () ->

                // Generate a choice type that calls `GetArrayChildrenByTypeTag`
                generateMultipleChoiceType ctx types true nameOverride (fun result tagCode ->
                    fun (Singleton jDoc) ->
                        // Generate method that calls `GetArrayChildrenByTypeTag`
                        let cultureStr = ctx.CultureStr

                        ctx.JsonRuntimeType?(nameof (JsonRuntime2.GetArrayChildrenByTypeTag))
                            (result.ConvertedTypeErased ctx)
                            (jDoc, cultureStr, tagCode, result.ConverterFunc ctx)))

        | InferedType.Heterogeneous (types, _) ->
            getOrCreateType ctx inferedType (fun () ->

                // Generate a choice type that always calls `TryGetValueByTypeTag`
                let types =
                    types
                    |> Map.map (fun _ v -> InferedMultiplicity.OptionalSingle, v)

                generateMultipleChoiceType ctx types false nameOverride (fun result tagCode ->
                    fun (Singleton jDoc) ->
                        let cultureStr = ctx.CultureStr

                        ctx.JsonRuntimeType?(nameof (JsonRuntime2.TryGetValueByTypeTag))
                            (result.ConvertedTypeErased ctx)
                            (jDoc, cultureStr, tagCode, result.ConverterFunc ctx)))

        | InferedType.Json _ -> failwith "Json type not supported"
