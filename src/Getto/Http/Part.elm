module Getto.Http.Part exposing
  ( Value
  , string
  , int
  , file
  , bytes
  , list
  , object
  , toBody
  )

{-| build http parts

    roles = [ "admin" ] |> Set.fromList
    comments = [ "good", "great" ]

    [ ( "name", "John" |> Part.string )
    , ( "age",  30     |> Part.int )
    , ( "file", file   |> Part.file )
    , ( "roles",    roles    |> Part.set Part.string )
    , ( "comments", comments |> Part.list Part.string )
    ] |> Part.object

# Definition
@docs Value

# Encoder
@docs string, int, file, bytes, list, object

# Encode
@docs toBody
 -}


import Getto.Url.Query.Encode as QueryEncode

import File exposing ( File )
import Bytes exposing ( Bytes )
import Http


{-| part value
 -}
type Value
  = StringValue String
  | FileValue File
  | BytesValue String Bytes
  | ListValue (List Value)
  | ObjectValue (List ( String, Value ))

type Part
  = StringPart String
  | FilePart File
  | BytesPart String Bytes


{-| string part
 -}
string : String -> Value
string = StringValue


{-| int part
 -}
int : Int -> Value
int = String.fromInt >> string


{-| file part
 -}
file : File -> Value
file = FileValue


{-| bytes part
 -}
bytes : String -> Bytes -> Value
bytes = BytesValue


{-| list part
 -}
list : (a -> Value) -> List a -> Value
list f = List.map f >> ListValue


{-| object part
 -}
object : List ( String, Value ) -> Value
object = ObjectValue


{-| convert value to http part
 -}
toBody : Value -> Http.Body
toBody =
  flatten []
  >> List.map toPart
  >> Http.multipartBody

flatten : List String -> Value -> List ( List String, Part )
flatten current part =
  case part of
    StringValue            value -> [ ( current, value |> StringPart ) ]
    FileValue              value -> [ ( current, value |> FilePart ) ]
    BytesValue contentType value -> [ ( current, value |> BytesPart contentType ) ]
    ListValue   parts -> parts |> List.concatMap (flatten (current ++ [""]))
    ObjectValue parts -> parts |> List.concatMap (\(name,p) -> p |> flatten (current ++ [name]))

toPart : ( List String, Part ) -> Http.Part
toPart (names,part) =
  let
    name = names |> QueryEncode.toName
  in
    case part of
      StringPart            value -> value |> Http.stringPart name
      FilePart              value -> value |> Http.filePart   name
      BytesPart contentType value -> value |> Http.bytesPart  name contentType
