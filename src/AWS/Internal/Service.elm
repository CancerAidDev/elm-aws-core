module AWS.Internal.Service exposing (Service, acceptType, contentType, host, region)

import AWS.Config exposing (ApiVersion, Endpoint, Protocol(..), Signer, TimestampFormat)


type alias Service =
    { endpointPrefix : String
    , apiVersion : ApiVersion
    , protocol : Protocol
    , signer : Signer
    , targetPrefix : String
    , timestampFormat : TimestampFormat
    , endpoint : Endpoint
    , jsonVersion : Maybe String
    , signingName : Maybe String
    , xmlNamespace : Maybe String
    , hostResolver : Endpoint -> String -> String
    , regionResolver : Endpoint -> String
    }


{-| Service endpoint as a hostname.
-}
host : Service -> String
host spec =
    spec.hostResolver spec.endpoint spec.endpointPrefix


{-| Service region.
-}
region : Service -> String
region { endpoint, regionResolver } =
    regionResolver endpoint


{-| Gets the service content type header value.
-}
contentType : Service -> String
contentType spec =
    case spec.protocol of
        EC2 ->
            "application/x-www-form-urlencoded"

        JSON ->
            case spec.jsonVersion of
                Just apiVersion ->
                    "application/x-amz-json-" ++ apiVersion

                Nothing ->
                    "application/json"

        QUERY ->
            "application/x-www-form-urlencoded"

        REST_JSON ->
            "application/json"

        REST_XML ->
            "application/xml"


{-| Gets the service Accept header value.
-}
acceptType : Service -> String
acceptType spec =
    case spec.protocol of
        REST_XML ->
            "application/xml"

        _ ->
            "application/json"
