#r "D:/Data/Dev/Archival-Mess/FSharp.Data/src/FSharp.Data/bin/Debug/netstandard2.0/FSharp.Data.dll"
#r "D:/Data/Dev/Archival-Mess/FSharp.Data/src/FSharp.Data/bin/Debug/netstandard2.0/FSharp.Data.DesignTime.dll"
#r "D:/Data/Dev/Archival-Mess/FSharp.Data/src/FSharp.Data/bin/Debug/netstandard2.0/FSharp.Data.Xml.Core.dll"
#r "D:/Data/Dev/Archival-Mess/FSharp.Data/src/FSharp.Data/bin/Debug/netstandard2.0/FSharp.Data.Json.Core.dll"
#r "D:/Data/Dev/Archival-Mess/FSharp.Data/src/FSharp.Data/bin/Debug/netstandard2.0/FSharp.Data.Csv.Core.dll"
#r "D:/Data/Dev/Archival-Mess/FSharp.Data/src/FSharp.Data/bin/Debug/netstandard2.0/FSharp.Data.Http.dll"
// #r "nuget: FSharp.Data.Xml.Core, 6.0.1-beta001"
// #r "nuget: FSharp.Data.Json.Core, 6.0.1-beta001"
// #r "nuget: FSharp.Data.Csv.Core, 6.0.1-beta001"
// #r "nuget: FSharp.Data.Http, 6.0.1-beta001"
// #r "nuget: FSharp.Data, 6.0.1-beta001"
// #r "nuget: FSharp.Data.Html.Core, 6.0.1-beta001"

open FSharp.Data
open System
open System.Globalization
open FSharp.Data.UnitSystems.SI.UnitNames
open System.Xml.Linq
open FSharp.Data.Runtime.StructuralInference
open System.Text.Json
open System.Text.Json.Serialization


// let test = System.Text.Json.JsonDocument.Parse(""" {"a":"1"} """, JsonDocumentOptions(CommentHandling=JsonCommentHandling.Skip, AllowTrailingCommas=true))
// let jsonVal = test.RootElement.GetProperty("a")
// let serializerOptions = JsonSerializerOptions(NumberHandling=JsonNumberHandling.AllowReadingFromString)
// System.Text.Json.JsonSerializer.Deserialize<float>(jsonVal, serializerOptions)
// jsonVal.Deserialize<float>(serializerOptions)


// let testNode = System.Text.Json.Nodes.JsonNode.Parse(""" {"a":1} """,documentOptions=JsonDocumentOptions(CommentHandling=JsonCommentHandling.Skip, AllowTrailingCommas=true))
// let x,y = testNode.AsObject().TryGetPropertyValue("a")
// y.GetValue<double>()

// let testNode2 = System.Text.Json.Nodes.JsonObject.Create(test.RootElement)
// testNode2.Parent

let csv1 = CsvFile.Parse("a,b,c\n , ,1\na,b,2")
csv1.Headers
csv1.Rows |> Seq.head

let csv = CsvProvider<"a,b,c\n , ,1\na,b,2",Schema=",string option,int option">.GetSample()
let first = csv.Rows |> Seq.head
first.A

type PT = JsonProvider<"""{"k": [["foo", 1], ["bar", 2]] }""">
// wrong inferred type:
let k1 = PT.GetSample().K


type OptionalString = JsonProvider<"""{"field": "asdf"} {}""", InferenceMode=InferenceMode.ValuesAndInlineSchemasOverrides, SampleIsList=true>
type OptionalStringNullable = JsonProvider<"""{"field": "asdf"} {"field": null}""", InferenceMode=InferenceMode.ValuesAndInlineSchemasOverrides, SampleIsList=true>
type StringEmpty = JsonProvider<"""{"field": "asdf"} {"field": ""}""", InferenceMode=InferenceMode.ValuesAndInlineSchemasOverrides, SampleIsList=true>
type MandatoryString = JsonProvider<"""{"field": "asdf"} {"field": "asdf"}""", InferenceMode=InferenceMode.ValuesAndInlineSchemasOverrides, SampleIsList=true>
OptionalString.Parse("""{"field":null}""").Field // None
OptionalString.Parse("""{}""").Field // None
OptionalStringNullable.Parse("""{"field":null}""").Field // None
OptionalStringNullable.Parse("""{}""").Field // None
StringEmpty.Parse("""{"field":null}""").Field // None
StringEmpty.Parse("""{}""").Field // None
MandatoryString.Parse("""{"field":null}""").Field // ""
MandatoryString.Parse("""{}""").Field // ""




// val t: JsonProvider<...>.Root = {
//   "not": "valid"
// }
// val it: string = ""

// System.Exception: '/field' is missing

type MyXml = XmlProvider<"""<item field="typeof{float}" />""",InferenceMode=InferenceMode.ValuesAndInlineSchemasOverrides>
let x = MyXml.Parse("""<item not="valid"/>""")
x.Field
// val x: XmlProvider<...>.Item = <item not="valid" />
// val it: string = ""

// System.Exception: Attribute field is missing

type MyCsv = CsvProvider<"""A,B""", Schema="string,int?">
let csvx = MyCsv.ParseRows("""1,""")
csvx.[0].B

type TestXml = XmlProvider<
    """
    <samples>
    
    <root>
        <error message="asdf" />
    </root>
    
    <root></root>
    
    <root>
        <data>
            <item id="1"/>
            <item id="2"/>
        </data>
    </root> 

    <root>
    <data/>
    </root>

    <!--
    
    
        <item><test>typeof{int{metre}}</test></item>  
        <item><test>typeof{bool}</test></item>
        <item><test/></item>
    <item/>
    <item><test>asdf</test></item>  
    <item><test>typeof{bool}</test></item>
    
    <item><test>10900</test></item>       
        <item><test/></item> 
        <item attr="asdf"><test>typeof{string}</test></item>  
        <item attr="1a"/>
        <item attr="typeof{int}"/>   
        <item attr="typeof{bool}"/>     
        -->
    
    </samples>
    """
    ,SampleIsList = true
    // ,InferenceMode = InferenceMode.ValuesOnly
    // ,InferenceMode = InferenceMode.ValuesAndInlineSchemasHints
    ,InferenceMode = InferenceMode.ValuesAndInlineSchemasOverrides
    >

let testXml =
    // TestXml.Parse("<root><item><test>12</test></item></root>")
    // TestXml.Parse("""<item test="yes"><test>1</test></item>""")
    (TestXml.GetSamples()[0])
        // .Items[0].Attr.Boolean
        .Data.Value.Items


type TestXml2 = XmlProvider<
    """
    <test aa="1">
        <child a="2" b="42"></child>
    </test>
    """
    >

let sample = TestXml2.GetSample()
sample.XElement.GetHashCode()
sample.Child.XElement.Parent.GetHashCode()

sample.XElement
sample.Child.XElement.SetValue("asdf")
sample.Child.XElement.SetAttributeValue("a", "coucou")
sample.Child.A

let lensSetChildBAttribute (root:TestXml2.Test) value =
    TestXml2.Test(aa=root.Aa,child=TestXml2.Child(a=root.Child.A,b=value))

let changed = lensSetChildBAttribute sample 43
changed.XElement.GetHashCode()
changed.Child.XElement.Parent.GetHashCode()

// let x = testXml()
// let y = x  + 2<metre>

let xxx = typeof<int<metre>>

// type ServiceResponse = JsonProvider<"""[
// { "code": 0, "value": {"generic payload": "yes"}, "message": null},
// { "code": 1, "value": null, "message": "Warning"},
// { "code": 2, "value": [], "message": "Exception"}   
// ]
// """
// , SampleIsList = true
// // ,InferenceMode = InferenceMode.ValuesOnly
// // ,InferenceMode = InferenceMode.ValuesAndInlineSchemasHints
// ,InferenceMode = InferenceMode.ValuesAndInlineSchemasOverrides>

// let test2 =
//     let root = ServiceResponse.GetSamples()[0]
//     root.Value.Record.Value.GenericPayload

type TestJson = JsonProvider<"""

    {
    "error": {
        "message": "Error validating access token: Session has expired on Friday, 24-Jul-20 16:00:00 PDT. The current time is Friday, 24-Jul-20 16:06:14 PDT."
    }
    }

    {
    "data": [
        {
        "id": "17841511906082530"
        },
        {
        "id": "17841511906082530"
        }
    ]
    } 

    // {
    //   "data": [

    //   ]
    // } 
    
    // {
    //   "data": null
    // } 
    
"""
    // ,InferenceMode = InferenceMode.ValuesOnly
    // ,InferenceMode = InferenceMode.ValuesAndInlineSchemasHints
    ,InferenceMode = InferenceMode.ValuesAndInlineSchemasOverrides
    ,SampleIsList = true >

let testJson =
    let item = (TestJson.GetSamples())[1]
    item.Data

let serializedJson =
    TestJson.Root(
        error = None, //Some (TestJson.Error(message = "asdf")),
        data = [| TestJson.Datum(id = 1) |]
    ).JsonValue.ToString()


let mapOption f =
    function
    | None -> null
    | Some v -> f v

let array = System.Linq.Enumerable.ToArray([| 1; 2; 3 |]) :> Array
let arrayOption = Some array
let arrayToList = [| for elem in array -> elem |]
let arrayOptionToList  = mapOption (fun (array: Array) -> [| for elem in array -> elem |]) arrayOption

// let value = [| 1; 2; 3|] |> Some |> box

// let optionType =
//   let optionType = value.GetType()
//   let optionTypeArg = optionType.GetGenericArguments()[0]
//   let info, fields = FSharp.Reflection.FSharpValue.GetUnionFields(value, optionType) 
//   match info.Name, fields with
//   | "Some", [| x |] -> Some (unbox x : Array)
//   | "None", _ -> None
//   | x, _ -> failwith x

// match value with
// | :? Array as v -> "array"
// | :? option<Array> as v -> "array option"
// | :? option<_> as v -> "array option ob"
// | :? option<'a> as v ->
//   match v with
//   | None -> "None option array"
//   | Some v ->
//     [for x in v -> ""] |> ignore
//     "does it work?"
// | typ when typ <> null && typ.GetType().IsGenericType -> "ok"
// | _ -> failwith $"fail {value.GetType()}"

// $"{(typedefof<option<_>>).IsConstructedGenericType }"
// $"{ value.GetType().GetGenericTypeDefinition() = (typedefof<option<_>>) }"
// $"{ (value.GetType().GetGenericArguments()[0]).BaseType = (typeof<Array>)  }"

// type TwitterJson =
//     JsonProvider<
//         @"D:\Data\Dev\Archival-Mess\FSharp.Data\tests\FSharp.Data.Tests\Data\TwitterStream.json"
//         , SampleIsList = true
//         , InferTypesFromValues = true
//         , PreferDictionaries = false>

// let test =
//     let user = 
//         TwitterJson.GetSamples()
//             .[0].User
//     let a = user.Value.ProfileLinkColor
//     let b = user.Value.ProfileTextColor
//     let c = user.Value.ProfileBackgroundColor
//     let d = user.Value.ProfileSidebarFillColor
//     let e = user.Value.ProfileSidebarBorderColor
//     a,b,c,d,e