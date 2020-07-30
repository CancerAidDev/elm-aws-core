module AWS.Http exposing
    ( send, sendUnsigned
    , Method(..), Path, Request
    , request
    , Body, MimeType
    , emptyBody, stringBody, jsonBody
    , addHeaders, addQuery
    , ResponseDecoder, HttpStatus(..)
    , fullDecoder, jsonFullDecoder, stringBodyDecoder, jsonBodyDecoder, constantDecoder
    )

{-| Handling of HTTP requests to AWS Services.


# Tasks for sending requests to AWS.

@docs send, sendUnsigned


# Build a Request

@docs Method, Path, Request
@docs request


# Build the HTTP Body of a Request

@docs Body, MimeType
@docs emptyBody, stringBody, jsonBody


# Add headers or query parameters to a Request

@docs addHeaders, addQuery


# Build decoders to interpret the response.

@docs ResponseDecoder, HttpStatus
@docs fullDecoder, jsonFullDecoder, stringBodyDecoder, jsonBodyDecoder, constantDecoder


# Error reporting.

-}

import AWS.Config exposing (Protocol(..), Signer(..))
import AWS.Credentials exposing (Credentials)
import AWS.Internal.Body
import AWS.Internal.Request exposing (Request, ResponseDecoder, ResponseStatus(..))
import AWS.Internal.Service as Service exposing (Service)
import AWS.Internal.Unsigned as Unsigned
import AWS.Internal.V4 as V4
import Http exposing (Metadata)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode
import Task exposing (Task)
import Time exposing (Posix)



--=== Tasks for sending requests to AWS.


{-| Signs and sends a `Request` to a `Service`.
-}
send :
    Service
    -> Credentials
    -> Request a
    -> Task.Task Http.Error a
send service credentials req =
    let
        prepareRequest : Request a -> Request a
        prepareRequest innerReq =
            case service.protocol of
                JSON ->
                    addHeaders
                        [ ( "x-amz-target", service.targetPrefix ++ "." ++ innerReq.name ) ]
                        innerReq

                _ ->
                    innerReq

        signWithTimestamp : Request a -> Posix -> Task Http.Error a
        signWithTimestamp innerReq posix =
            case service.signer of
                SignV4 ->
                    V4.sign service credentials posix innerReq

                SignS3 ->
                    Task.fail (Http.BadBody "TODO: S3 Signing Scheme not implemented.")
    in
    Time.now |> Task.andThen (prepareRequest req |> signWithTimestamp)


{-| Sends a `Request` to a `Service` without signing it.
-}
sendUnsigned :
    Service
    -> Request a
    -> Task.Task Http.Error a
sendUnsigned service req =
    let
        prepareRequest : Request a -> Request a
        prepareRequest innerReq =
            case service.protocol of
                JSON ->
                    addHeaders
                        [ ( "x-amz-target", service.targetPrefix ++ "." ++ innerReq.name ) ]
                        innerReq

                _ ->
                    innerReq

        withTimestamp : Request a -> Posix -> Task Http.Error a
        withTimestamp innerReq posix =
            Unsigned.prepare service posix innerReq
    in
    Time.now |> Task.andThen (prepareRequest req |> withTimestamp)



--=== Build a request


{-| Holds an unsigned AWS HTTP request.
-}
type alias Request a =
    AWS.Internal.Request.Request a


{-| HTTP request methods.
-}
type Method
    = DELETE
    | GET
    | HEAD
    | OPTIONS
    | POST
    | PUT


{-| Request path.
-}
type alias Path =
    String


{-| Creates an unsigned HTTP request to an AWS service.
-}
request :
    String
    -> Method
    -> Path
    -> Body
    -> ResponseDecoder a
    -> Request a
request name method path body decoder =
    AWS.Internal.Request.unsigned name (methodToString method) path body decoder



--=== Build th HTTP Body of a Request


{-| Holds a request body.
-}
type alias Body =
    AWS.Internal.Body.Body


{-| MIME type.

See <https://en.wikipedia.org/wiki/Media_type>

-}
type alias MimeType =
    String


{-| Create an empty body.
-}
emptyBody : Body
emptyBody =
    AWS.Internal.Body.empty


{-| Create a body containing a JSON value.

This will automatically add the `Content-Type: application/json` header.

-}
jsonBody : Json.Encode.Value -> Body
jsonBody =
    AWS.Internal.Body.json


{-| Create a body with a custom MIME type and the given string as content.

    stringBody "text/html" "<html><body><h1>Hello</h1></body></html>"

-}
stringBody : MimeType -> String -> Body
stringBody =
    AWS.Internal.Body.string



--=== Add headers or query parameters to a Request


{-| Appends headers to an AWS HTTP unsigned request.

See the `AWS.KVEncode` for encoder functions to build the headers with.

-}
addHeaders : List ( String, String ) -> Request a -> Request a
addHeaders headers req =
    { req | headers = List.append req.headers headers }


{-| Appends query arguments to an AWS HTTP unsigned request.

See the `AWS.KVEncode` for encoder functions to build the query parameters with.

-}
addQuery : List ( String, String ) -> Request a -> Request a
addQuery query req =
    { req | query = List.append req.query query }



--=== Build decoders to interpret the response.


{-| Decoders that interpret responses.
-}
type alias ResponseDecoder a =
    AWS.Internal.Request.ResponseDecoder a


{-| The HTTP response code type according to how `Elm.Http` classifies responses.

A code from 200 to less than 300 is considered 'Good' and any other code is
considered 'Bad'.

-}
type HttpStatus
    = GoodStatus
    | BadStatus


httpStatus : ResponseStatus -> HttpStatus
httpStatus status =
    case status of
        GoodStatus_ ->
            GoodStatus

        BadStatus_ ->
            BadStatus


{-| A full decoder for the response that can look at the status code, metadata
including headers and so on. The body is presented as a `String` for parsing.

It is possible to report an error as a String when interpreting the response, and
this will be mapped onto `Http.BadBody` when present.

-}
fullDecoder : (HttpStatus -> Metadata -> String -> Result String a) -> ResponseDecoder a
fullDecoder decodeFn =
    \status metadata body ->
        case decodeFn (httpStatus status) metadata body of
            Ok val ->
                Ok val

            Err err ->
                Http.BadBody err |> Err


{-| A full JSON decoder for the response that can look at the status code, metadata
including headers and so on. The body is presented as a JSON `Value` for decoding.

Any decoder error is mapped onto `Http.BadBody` as a `String` when present using
`Decode.errorToString`.

-}
jsonFullDecoder : (HttpStatus -> Metadata -> Decoder a) -> ResponseDecoder a
jsonFullDecoder decodeFn =
    \status metadata body ->
        case Decode.decodeString (decodeFn (httpStatus status) metadata) body of
            Ok val ->
                Ok val

            Err err ->
                Http.BadBody (Decode.errorToString err) |> Err


{-| A decoder for the response that uses only the body presented as a `String`
for parsing.

It is possible to report an error as a String when interpreting the response, and
this will be mapped onto `Http.BadBody` when present.

Note that this decoder is only used when the response is Http.GoodStatus\_. An
Http.BadStatus\_ is always mapped to Http.BadStatus without attempting to decode
the body. If you need to handle things that Elm HTTP regards as BadStatus\_, use
one of the 'full' decoders.

-}
stringBodyDecoder : (String -> Result String a) -> ResponseDecoder a
stringBodyDecoder decodeFn =
    \status metadata body ->
        case status of
            GoodStatus_ ->
                case decodeFn body of
                    Ok val ->
                        Ok val

                    Err err ->
                        Http.BadBody err |> Err

            BadStatus_ ->
                Http.BadStatus metadata.statusCode |> Err


{-| A decoder for the response that uses only the body presented as a JSON `Value`
for decoding.

Any decoder error is mapped onto `Http.BadBody` as a `String` when present using
`Decode.errorToString`.

Note that this decoder is only used when the response is Http.GoodStatus\_. An
Http.BadStatus\_ is always mapped to Http.BadStatus without attempting to decode
the body. If you need to handle things that Elm HTTP regards as BadStatus\_, use
one of the 'full' decoders.

-}
jsonBodyDecoder : Decoder a -> ResponseDecoder a
jsonBodyDecoder decodeFn =
    \status metadata body ->
        case status of
            GoodStatus_ ->
                case Decode.decodeString decodeFn body of
                    Ok val ->
                        Ok val

                    Err err ->
                        Http.BadBody (Decode.errorToString err) |> Err

            BadStatus_ ->
                Http.BadStatus metadata.statusCode |> Err


{-| Not all AWS service produce a response that contains useful information.

The `constantDecoder` is helpful in those situations and just produces whatever
value you give it once AWS has responded.

Note that this decoder is only used when the response is Http.GoodStatus\_. An
Http.BadStatus\_ is always mapped to Http.BadStatus without attempting to decode
the body. If you need to handle things that Elm HTTP regards as BadStatus\_, use
one of the 'full' decoders.

-}
constantDecoder : a -> ResponseDecoder a
constantDecoder val =
    \status metadata _ ->
        case status of
            GoodStatus_ ->
                Ok val

            BadStatus_ ->
                Http.BadStatus metadata.statusCode |> Err



--=== Error reporting.


{-| AWS reports errors at the API level as 'exceptions' with a 400 error code.
The format of these errors is always the same. An example in JSON is:

    { "\_\_type":"PasswordResetRequiredException"
    , "message":"Password reset required for the user"
    }

The raw error from AWS is placed in the `raw` field. An interpretation of this
by your error handler will be placed in the `error` field.

-}
type alias AWSError a =
    { error : a
    , raw : ErrorResponse
    }


{-| The raw error fields returned from an AWS API call.
-}
type alias ErrorResponse =
    { type_ : String
    , message : String
    }



--=== Helper functions.


methodToString : Method -> String
methodToString meth =
    case meth of
        DELETE ->
            "DELETE"

        GET ->
            "GET"

        HEAD ->
            "HEAD"

        OPTIONS ->
            "OPTIONS"

        POST ->
            "POST"

        PUT ->
            "PUT"
