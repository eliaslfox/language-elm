module Maybe exposing (Maybe(Just, Nothing), andThen, map, map2, map3, map4, map5, withDefault)


type Maybe a
    = Nothing
    | Just a


withDefault : a -> Maybe a -> a
withDefault default maybe =
    case maybe of
        Just value ->
            value

        Nothing ->
            default


map : (a -> b) -> Maybe a -> Maybe b
map f maybe =
    case maybe of
        Just value ->
            Just (f value)

        Nothing ->
            Nothing


map2 : (a -> b -> value) -> Maybe a -> Maybe b -> Maybe value
map2 func ma mb =
    case ( ma, mb ) of
        ( Just a, Just b ) ->
            Just (func a b)

        _ ->
            Nothing


map3 : (a -> b -> c -> value) -> Maybe a -> Maybe b -> Maybe c -> Maybe value
map3 func ma mb mc =
    case ( ma, mb, mc ) of
        ( Just a, Just b, Just c ) ->
            Just (func a b c)

        _ ->
            Nothing
