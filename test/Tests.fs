module Tests

open System
open Xunit

open FParsec
open JsonParser
open JsonValue
open System.IO

let test (parser: Parser<'a>) str (expect: 'a) =
    match run parser str with
    | Success(r,_,_) -> Assert.Equal<'a>(expect, r)
    | Failure(m,_,_) -> Assert.True(false, m)


[<Fact>]
let ``Float test`` () =
    let expect = FloatValue -0.01

    test numberValue "-0.1E-1" expect

[<Fact>]
let ``String literal test`` () =
    let expect = StringValue "\n\b\u1234"

    test stringValue "\"\\n\\b\\u1234\"" expect

[<Fact>]
let ``Object test`` () =
    let testStr = "{
        \"A\": [
            1, 3, -5, -0.1E-1
        ],
        \"str\": \"\\n\\b\\u1234\"
    }"

    let expect =
        ObjectValue << Map <|
            [
                "A", ArrayValue 
                    [
                        IntegerValue 1
                        IntegerValue 3
                        IntegerValue -5
                        FloatValue -0.01
                    ]
                "str", StringValue
                    "\n\b\u1234"
            ]

    
    test objectValue testStr expect