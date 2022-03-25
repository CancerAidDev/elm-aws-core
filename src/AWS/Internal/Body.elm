module AWS.Internal.Body exposing
    ( Body
    , empty
    , json
    , string
    , toHttp
    , toString
    )

import AWS.Internal.Service exposing (Service, contentType)
import Http
import Json.Encode


type Body
    = Empty
    | Json Json.Encode.Value
    | String String String


toHttp : Service -> Body -> Http.Body
toHttp service body =
    case body of
        Empty ->
            Http.emptyBody

        Json value ->
            Http.stringBody (contentType service) (Json.Encode.encode 0 value)

        String mimeType val ->
            Http.stringBody mimeType val


toString : Body -> String
toString body =
    case body of
        Json value ->
            Json.Encode.encode 0 value

        Empty ->
            ""

        String _ val ->
            val


empty : Body
empty =
    Empty


json : Json.Encode.Value -> Body
json =
    Json


string : String -> String -> Body
string =
    String
