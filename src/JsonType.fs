module JsonValue

type JsonValue =
    | NullValue
    | StringValue of string
    | IntegerValue of int
    | FloatValue of float
    | TrueValue | FalseValue
    | ArrayValue of JsonValue list
    | ObjectValue of Map<string, JsonValue>
