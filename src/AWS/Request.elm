module AWS.Request exposing
    ( Unsigned
    , queryString
    , unsigned
    , url
    )

{-| Internal representation of a request.
-}

import AWS.Body as Body exposing (Body)
import AWS.QueryString as QueryString
import AWS.Service as Service exposing (Service)
import AWS.Uri
import Http
import Json.Decode exposing (Decoder)


type alias Unsigned a =
    { name : String
    , method : String
    , path : String
    , body : Body
    , headers : List ( String, String )
    , query : List ( String, String )

    -- when dealing with the body only.
    , decoder : String -> Result String a

    -- more generally over the whole response
    , responseParser : Maybe (Http.Response String -> Result Http.Error a)

    -- need a way to get headers + response code with decoder.
    -- headers will be a List (String, String)
    }


unsigned :
    String
    -> String
    -> String
    -> Body
    -> (String -> Result String a)
    -> Unsigned a
unsigned name method uri body decoder =
    Unsigned
        name
        method
        uri
        body
        decoder
        []
        []
        Nothing


url : Service -> Unsigned a -> String
url service { path, query } =
    "https://"
        ++ Service.host service
        ++ path
        ++ queryString query


queryString : List ( String, String ) -> String
queryString params =
    case params of
        [] ->
            ""

        _ ->
            params
                |> List.foldl
                    (\( key, val ) qs ->
                        qs |> QueryString.add (AWS.Uri.percentEncode key) val
                    )
                    QueryString.empty
                |> QueryString.render
