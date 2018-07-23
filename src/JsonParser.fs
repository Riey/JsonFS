module JsonParser

open System

open JsonValue
open FParsec

type Parser<'a> = Parser<'a, unit>

let ws : Parser<_> = spaces

let LSB = pchar '[' .>> ws
let RSB = pchar ']' .>> ws
let LCB = pchar '{' .>> ws
let RCB = pchar '}' .>> ws
let COMMA = pchar ',' .>> ws
let COLON = pchar ':' .>> ws

let jsonNumberOptions =
    NumberLiteralOptions.AllowFraction |||
    NumberLiteralOptions.AllowFractionWOIntegerPart |||
    NumberLiteralOptions.AllowMinusSign |||
    NumberLiteralOptions.AllowExponent

let numberValue =
    numberLiteral jsonNumberOptions "jsonNumber" 
        |>> fun nl ->
            match nl.HasFraction with
            | true -> nl.String |> float |> FloatValue
            | false -> nl.String |> int |> IntegerValue

let jsonString =
    let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
    let unescape c = match c with
                     | 'n' -> '\n'
                     | 'r' -> '\r'
                     | 't' -> '\t'
                     | 'f' -> '\f'
                     | 'b' -> '\b'
                     | c   -> c
    let simpleEscapeChar = "\"\\/bfnrt" |> anyOf |>> unescape
    let digitEscapeChar = pchar 'u' >>. manyMinMaxSatisfy 4 4 isHex |>> fun s -> Convert.ToInt32(s, 16) |> char
    let escapeChar = pchar '\\' >>. (simpleEscapeChar <|> digitEscapeChar)
    between (pchar '"') (pchar '"')
        (manyChars (normalChar <|> escapeChar))

let stringValue = 
    jsonString |>> StringValue

let trueValue = pstring "true" |>> fun _ -> TrueValue

let falseValue = pstring "false" |>> fun _ -> FalseValue

let nullValue = pstring "null" |>> fun _ -> NullValue

let jsonValue, jsonValueImpl = createParserForwardedToRef()

let arrayValue =
    between LSB RSB (sepBy (jsonValue .>> ws) COMMA) |>> ArrayValue

let objectValue =
    let object = ws >>. jsonString .>> ws .>> COLON .>>. jsonValue
    between LCB RCB (sepBy object COMMA) |>> (Map >> ObjectValue) 

jsonValueImpl :=
    ws >>. choice
        [|
            trueValue
            falseValue
            nullValue
            stringValue
            numberValue
            objectValue
            arrayValue
        |] .>> ws