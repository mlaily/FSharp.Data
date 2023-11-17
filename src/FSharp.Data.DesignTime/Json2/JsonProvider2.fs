namespace ProviderImplementation

open System
open System.IO
open FSharp.Core.CompilerServices
open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProviderHelpers
open FSharp.Data
open FSharp.Data.Runtime
open FSharp.Data.Runtime.BaseTypes
open FSharp.Data.Runtime.StructuralTypes
open FSharp.Data.Runtime.StructuralInference
open System.Net
open System.Text.Json.Nodes

// ----------------------------------------------------------------------------------------------

#nowarn "10001"

[<TypeProvider>]
type public JsonProvider2(cfg: TypeProviderConfig) as this =
    inherit DisposableTypeProviderForNamespaces
        (
            cfg,
            assemblyReplacementMap = [ "FSharp.Data.DesignTime", "FSharp.Data" ]
        )

    // Generate namespace and type 'FSharp.Data.JsonProvider2'
    do AssemblyResolver.init ()
    let asm = System.Reflection.Assembly.GetExecutingAssembly()
    let ns = "FSharp.Data"

    let jsonProvTy =
        ProvidedTypeDefinition(asm, ns, "JsonProvider2", None, hideObjectMethods = true, nonNullable = true)

    let buildTypes (typeName: string) (args: obj[]) =

        // Enable TLS 1.2 for samples requested through https.
        ServicePointManager.SecurityProtocol <-
            ServicePointManager.SecurityProtocol
            ||| SecurityProtocolType.Tls12

        // Generate the required type
        let tpType =
            ProvidedTypeDefinition(asm, ns, typeName, None, hideObjectMethods = true, nonNullable = true)

        let sample = args.[0] :?> string
        let sampleIsList = args.[1] :?> bool
        let rootName = args.[2] :?> string

        let rootName =
            if String.IsNullOrWhiteSpace rootName then
                "Root"
            else
                NameUtils.singularize rootName

        let cultureStr = args.[3] :?> string
        let encodingStr = args.[4] :?> string
        let resolutionFolder = args.[5] :?> string
        let resource = args.[6] :?> string
        let preferDictionaries = args.[7] :?> bool
        let inferenceMode = args.[8] :?> InferenceMode

        let inferenceMode =
            InferenceMode'.FromPublicApi(inferenceMode, legacyInferTypesFromValues = true)

        let cultureInfo = TextRuntime.GetCulture cultureStr
        let unitsOfMeasureProvider = ProviderHelpers.unitsOfMeasureProvider

        let getSpec _ value =

            let samples =
                use _holder = IO.logTime "Parsing" sample

                if sampleIsList then
                    JsonWrapper.CreateList(new StringReader(value))
                    |> Array.map (fun doc -> doc.JsonNode)
                else
                    [| JsonWrapper.ParseNode(value) |]

            let inferedType =
                use _holder = IO.logTime "Inference" sample

                samples
                |> Array.map (fun sampleJson ->
                    JsonInference2.inferType unitsOfMeasureProvider inferenceMode cultureInfo "" sampleJson)
                |> Array.fold (StructuralInference.subtypeInfered false) InferedType.Top

            use _holder = IO.logTime "TypeGeneration" sample

            let ctx =
                JsonGenerationContext2.Create(
                    cultureStr,
                    tpType,
                    unitsOfMeasureProvider,
                    inferenceMode,
                    ?preferDictionaries = Some preferDictionaries
                )

            let result = JsonTypeBuilder2.generateJsonType ctx false false rootName inferedType

            { GeneratedType = tpType
              RepresentationType = result.RepresentationType
              CreateFromTextReader = fun reader -> result.Convert <@@ JsonWrapper.Create(%reader) @@>
              CreateListFromTextReader = Some(fun reader -> result.Convert <@@ JsonWrapper.CreateList(%reader) @@>)
              CreateFromTextReaderForSampleList = fun reader -> result.Convert <@@ JsonWrapper.CreateList(%reader) @@>
              CreateFromValue = Some(typeof<JsonNode>, (fun value -> result.Convert <@@ JsonWrapper(%value) @@>)) }

        let source = if sampleIsList then SampleList sample else Sample sample

        generateType "JSON" source getSpec this cfg encodingStr resolutionFolder resource typeName None

    // Add static parameter that specifies the API we want to get (compile-time)
    let parameters =
        [ ProvidedStaticParameter("Sample", typeof<string>)
          ProvidedStaticParameter("SampleIsList", typeof<bool>, parameterDefaultValue = false)
          ProvidedStaticParameter("RootName", typeof<string>, parameterDefaultValue = "Root")
          ProvidedStaticParameter("Culture", typeof<string>, parameterDefaultValue = "")
          ProvidedStaticParameter("Encoding", typeof<string>, parameterDefaultValue = "")
          ProvidedStaticParameter("ResolutionFolder", typeof<string>, parameterDefaultValue = "")
          ProvidedStaticParameter("EmbeddedResource", typeof<string>, parameterDefaultValue = "")
          ProvidedStaticParameter("PreferDictionaries", typeof<bool>, parameterDefaultValue = false)
          ProvidedStaticParameter(
              "InferenceMode",
              typeof<InferenceMode>,
              parameterDefaultValue = InferenceMode.ValuesAndInlineSchemasOverrides
          ) ]

    let helpText =
        """<summary>Typed representation of a JSON document.</summary>
           <param name='Sample'>Location of a JSON sample file or a string containing a sample JSON document.</param>
           <param name='SampleIsList'>If true, sample should be a list of individual samples for the inference.</param>
           <param name='RootName'>The name to be used to the root type. Defaults to `Root`.</param>
           <param name='Culture'>The culture used for parsing numbers and dates. Defaults to the invariant culture.</param>
           <param name='Encoding'>The encoding used to read the sample. You can specify either the character set name or the codepage number. Defaults to UTF8 for files, and to ISO-8859-1 the for HTTP requests, unless `charset` is specified in the `Content-Type` response header.</param>
           <param name='ResolutionFolder'>A directory that is used when resolving relative file references (at design time and in hosted execution).</param>
           <param name='EmbeddedResource'>When specified, the type provider first attempts to load the sample from the specified resource 
              (e.g. 'MyCompany.MyAssembly, resource_name.json'). This is useful when exposing types generated by the type provider.</param>
           <param name='InferTypesFromValues'>
              This parameter is deprecated. Please use InferenceMode instead.
              If true, turns on additional type inference from values. 
              (e.g. type inference infers string values such as "123" as ints and values constrained to 0 and 1 as booleans.)</param>
           <param name='PreferDictionaries'>If true, json records are interpreted as dictionaries when the names of all the fields are inferred (by type inference rules) into the same non-string primitive type.</param>
           <param name='InferenceMode'>Possible values:
              | NoInference -> Inference is disabled. All values are inferred as the most basic type permitted for the value (i.e. string or number or bool).
              | ValuesOnly -> Types of values are inferred from the Sample. Inline schema support is disabled.
              | ValuesAndInlineSchemasHints -> Types of values are inferred from both values and inline schemas. Inline schemas are special string values that can define a type and/or unit of measure. Supported syntax: typeof&lt;type&gt; or typeof{type} or typeof&lt;type&lt;measure&gt;&gt; or typeof{type{measure}}. Valid measures are the default SI units, and valid types are <c>int</c>, <c>int64</c>, <c>bool</c>, <c>float</c>, <c>decimal</c>, <c>date</c>, <c>datetimeoffset</c>, <c>timespan</c>, <c>guid</c> and <c>string</c>.
              | ValuesAndInlineSchemasOverrides -> Same as ValuesAndInlineSchemasHints, but value inferred types are ignored when an inline schema is present. This is the default.
           </param>"""

    do jsonProvTy.AddXmlDoc helpText
    do jsonProvTy.DefineStaticParameters(parameters, buildTypes)

    // Register the main type with F# compiler
    do this.AddNamespace(ns, [ jsonProvTy ])
