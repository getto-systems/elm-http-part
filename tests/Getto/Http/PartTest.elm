module Getto.Http.PartTest exposing (..)
import Getto.Http.Part as Part

import Http

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

suite : Test
suite =
  describe "Part"
    [ describe "toBody"
      [ test "should encode params" <|
        \_ ->
          let
            value =
              [ ( "q"
                , [ ( "name", "John" |> Part.string )
                  , ( "roles"
                    , [ "admin"
                      , "system"
                      ] |> Part.list Part.string
                    )
                  ] |> Part.object
                )
              ] |> Part.object
          in
            value |> Part.toBody
            |> Expect.equal
              (
                [ Http.stringPart "q[name]" "John"
                , Http.stringPart "q[roles][]" "admin"
                , Http.stringPart "q[roles][]" "system"
                ] |> Http.multipartBody
              )
      ]
    ]
