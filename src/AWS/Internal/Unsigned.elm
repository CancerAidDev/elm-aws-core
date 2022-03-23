module AWS.Internal.Unsigned exposing (prepare)

{-| Unsigned request implementation.
-}

import AWS.Internal.Body exposing (Body)
import AWS.Internal.Canonical exposing (canonicalPayload)
import AWS.Internal.Error as Error
import AWS.Internal.Request exposing (Request)
import AWS.Internal.Service as Service exposing (Service)
import AWS.Internal.UrlBuilder
import Http
import Iso8601
import Regex
import Task exposing (Task)
import Time exposing (Posix)


{-| Prepares the request without signing it.
-}
prepare :
    Service
    -> Posix
    -> Request err a
    -> Task (Error.Error err) a
prepare service date req =
    let
        responseDecoder response =
            case response of
                Http.BadUrl_ url ->
                    Http.BadUrl url
                        |> Error.HttpError
                        |> Err

                Http.Timeout_ ->
                    Http.Timeout
                        |> Error.HttpError
                        |> Err

                Http.NetworkError_ ->
                    Http.NetworkError
                        |> Error.HttpError
                        |> Err

                Http.BadStatus_ metadata body ->
                    case req.errorDecoder metadata body of
                        Ok appErr ->
                            Error.AWSError appErr
                                |> Err

                        Err err ->
                            Http.BadBody err
                                |> Error.HttpError
                                |> Err

                Http.GoodStatus_ metadata body ->
                    case req.decoder metadata body of
                        Ok resp ->
                            Ok resp

                        Err err ->
                            Http.BadBody err
                                |> Error.HttpError
                                |> Err

        resolver =
            Http.stringResolver responseDecoder
    in
    Http.task
        { method = req.method
        , headers =
            headers service date req.body req.headers
                |> List.map (\( key, val ) -> Http.header key val)
        , url = AWS.Internal.UrlBuilder.url service req
        , body = AWS.Internal.Body.toHttp service req.body
        , resolver = resolver
        , timeout = Nothing
        }


headers : Service -> Posix -> Body -> List ( String, String ) -> List ( String, String )
headers service date body extraHeaders =
    let
        extraNames =
            List.map Tuple.first extraHeaders
                |> List.map String.toLower
    in
    List.concat
        [ extraHeaders
        , [ ( "x-amz-date", formatPosix date )
          , ( "x-amz-content-sha256", canonicalPayload body )
          ]
        , if List.member "accept" extraNames then
            []

          else
            [ ( "Accept", Service.acceptType service ) ]
        ]


formatPosix : Posix -> String
formatPosix date =
    date
        |> Iso8601.fromTime
        |> Regex.replace
            (Regex.fromString "([-:]|\\.\\d{3})" |> Maybe.withDefault Regex.never)
            (\_ -> "")
