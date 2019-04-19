module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import SmartUrl
import Url exposing (Protocol(..))


suite : Test
suite =
  describe "Smart url parsing"
    [ describe "SmartUrl.fromString"
      [ test "Simple, incomplete url" <|
          \_ -> 
            "google.com"
            |> SmartUrl.fromString
            |> Expect.equal (Url.fromString "http://google.com")
          
      , test "Url with protocol" <|
          \_ ->
            "https://google.com"
            |> SmartUrl.fromString 
            |> Expect.equal (Url.fromString "https://google.com")

      ,test "Incomplete url with port" <|
          \_ ->
            "google.com:8080"
            |> SmartUrl.fromString 
            |> Expect.equal (Url.fromString "http://google.com:8080")

      ,test "Incomplete url with path" <|
          \_ ->
            "google.com/path/to/page"
            |> SmartUrl.fromString 
            |> Expect.equal (Url.fromString "http://google.com/path/to/page")
          
      ,test "Incomplete url with port and path" <|
          \_ ->
            "google.com:8080/path/to/page"
            |> SmartUrl.fromString 
            |> Expect.equal (Url.fromString "http://google.com:8080/path/to/page")

      ,test "Incomplete url with query" <|
          \_ ->
            "google.com?search=cats&dogs"
            |> SmartUrl.fromString 
            |> Expect.equal (Url.fromString "http://google.com?search=cats&dogs")

      ,test "Incomplete url with port,path,query" <|
          \_ ->
            "google.com:8080/path/to/page?search=cats&dogs"
            |> SmartUrl.fromString 
            |> Expect.equal (Url.fromString "http://google.com:8080/path/to/page?search=cats&dogs")

      ,test "Incomplete url with fragment" <|
          \_ ->
            "google.com#linkInPage"
            |> SmartUrl.fromString 
            |> Expect.equal (Url.fromString "http://google.com#linkInPage")

      ,test "Incomplete url with port,path,query,fragment" <|
          \_ ->
            "google.com:8080/path/to/page?search=cats&dogs#linkInPage"
            |> SmartUrl.fromString 
            |> Expect.equal (Url.fromString "http://google.com:8080/path/to/page?search=cats&dogs#linkInPage")

      ,test "Complete url with port,path,query,fragment" <|
          \_ ->
            "https://google.com:8080/path/to/page?search=cats&dogs#linkInPage"
            |> SmartUrl.fromString 
            |> Expect.equal (Url.fromString "https://google.com:8080/path/to/page?search=cats&dogs#linkInPage")

      ,test "Complete url with small hostname" <|
          \_ ->
            "https://google"
            |> SmartUrl.fromString 
            |> Expect.equal (Url.fromString "https://google")

      ,test "Not enough info to determine link" <|
          \_ ->
            "google"
            |> SmartUrl.fromString 
            |> Expect.equal Nothing

      ,test "Invalid hostname" <|
          \_ ->
            "google@google.com"
            |> SmartUrl.fromString 
            |> Expect.equal Nothing

      ,test "Protocol with no hostname" <|
          \_ ->
            "http://"
            |> SmartUrl.fromString 
            |> Expect.equal Nothing

      ,test "Incomplete url with invalid hostname and port,query,fragment" <|
          \_ ->
            "google:8080/path/to/page?search=cats&dogs#linkInPage"
            |> SmartUrl.fromString 
            |> Expect.equal Nothing
      ]
    ]
