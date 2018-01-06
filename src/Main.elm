module Main exposing (..)

import Date exposing (Date)
import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src, class)


---- MODEL ----


type alias Model =
    { yearsCount : Int
    , birthDate : Maybe Date
    }

yearsList: Int -> List Int
yearsList =
    List.range 0


init : ( Model, Cmd Msg )
init =
    ( { yearsCount = 80
      , birthDate = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----

viewWeek: Model -> Html Msg
viewWeek model = div [ class "calendar__week" ] []

viewYear : Model -> Html Msg
viewYear model =
    div [ class "calendar__year" ]
        (List.map (\x -> viewWeek model) (List.range 1 52))


view : Model -> Html Msg
view model =
    div [ class "calendar" ]
        (List.map
            (\x -> viewYear model)
            (yearsList model.yearsCount)
        )



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
