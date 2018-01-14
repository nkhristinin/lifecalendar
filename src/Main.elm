module Main exposing (..)

import Date exposing (Date)
import Task
import Dict
import Html exposing (Html, text, div, h1, img, button, input, label, select, option, p, span, br, b)
import Html.Attributes exposing (src, class, classList, type_, value, placeholder)
import Html.Events exposing (..)
import Dropdown


---- MODEL ----


type alias Week =
    { index : Int
    , filled : Bool
    }


type alias Model =
    { yearsCount : Int
    , birthYear : Maybe Int
    , birthMonth : Maybe Int
    , birthDay : Maybe Int
    , weeks : Int
    , dateNow : Maybe Date
    , dropdownModel : Dropdown.Model
    }


weekList : Int -> List Week
weekList year =
    List.map (\x -> Week x False)
        (List.range 1 (year * weekInYear))


init : ( Model, Cmd Msg )
init =
    ( { yearsCount = 70
      , birthYear = Nothing
      , birthMonth = Nothing
      , birthDay = Nothing
      , weeks = 0
      , dateNow = Nothing
      , dropdownModel = Dropdown.initialModel
      }
    , Task.perform GetDateNow Date.now
    )

weekInYear: Int
weekInYear =
    52

months: List (String, Int)
months =
    [ ( "January", 1 )
    , ( "February", 2 )
    , ( "March", 3 )
    , ( "April", 4 )
    , ( "May", 5 )
    , ( "June", 6 )
    , ( "July", 7 )
    , ( "August", 8 )
    , ( "September", 9 )
    , ( "October", 10 )
    , ( "November", 11 )
    , ( "December", 12 )
    ]


split : Int -> List a -> List (List a)
split i list =
    case List.take i list of
        [] ->
            []

        listHead ->
            listHead :: split i (List.drop i list)



---- UPDATE ----


type Msg
    = GetDateNow Date
    | SetBirthYear String
    | SetBirthMonth String
    | SetBirthDay String
    | ShowCalendar
    | DropdownMsg Dropdown.Msg


toInt: String -> Maybe Int
toInt stringValue =
    stringValue
        |> String.toInt
        |> Result.toMaybe


getWeeks : Date -> Int -> Int -> Int -> Int
getWeeks now year month day =
    let
        yearWeeks =
            ((Date.year now) - year) * weekInYear

        monthWeeks =
            (month - 1) * 4

        dayWeeks =
            day // 7
    in
        List.foldl (+) 0 [ yearWeeks, monthWeeks, dayWeeks ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetDateNow date ->
            ( { model | dateNow = Just date }, Cmd.none )

        SetBirthYear year ->
            if (String.length year) == 4 then
                { model | birthYear = toInt year }
                    |> update ShowCalendar
            else
                ( model, Cmd.none )

        SetBirthMonth month ->
            { model | birthMonth = toInt month }
                |> update ShowCalendar

        SetBirthDay day ->
            { model | birthDay = toInt day }
                |> update ShowCalendar

        ShowCalendar ->
            case (Maybe.map4 getWeeks model.dateNow model.birthYear model.birthMonth model.birthDay) of
                Just weeks ->
                    ( { model | weeks = weeks }, Cmd.none )

                Nothing ->
                    ( { model | weeks = 0 }, Cmd.none )

        DropdownMsg msg ->
            let
                dropdownModel =
                    Dropdown.update msg model.dropdownModel
            in
                { model
                    | dropdownModel = dropdownModel
                    , birthMonth = (Dict.get (dropdownModel.selectedValue |> Maybe.withDefault "") (Dict.fromList months))
                }
                    |> update ShowCalendar



---- VIEW ----


viewField : Model -> String -> String -> (String -> Msg) -> Html Msg
viewField model placehold className msg =
    input [ onInput msg, placeholder placehold, class ("input " ++ className) ] []


viewWeek : Model -> Week -> Html Msg
viewWeek model week =
    div [ class "calendar__week", classList [ ( "calendar__week--filled", week.filled ) ] ] []


viewYear : Model -> List Week -> Html Msg
viewYear model year =
    div [ class "calendar__year" ]
        (List.map (viewWeek model) year)


viewCalendar : Model -> Html Msg
viewCalendar model =
    div [ class "calendar" ]
        (weekList model.yearsCount
            |> List.map (\week -> ({ week | filled = week.index <= model.weeks }))
            |> split weekInYear
            |> List.map (\year -> viewYear model year)
            |> List.append [viewDirections]
        )


viewDirections : Html Msg
viewDirections =
    div [ class "directions" ]
        [ div [ class "direction direction__week" ] [ text "Weeks" ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ p []
            [ text ("Imagine that the average life expectancy is ")
            , b [] [ text (toString model.yearsCount) ]
            , text (" years. This is approximately ")
            , b [] [ text (toString (model.yearsCount * weekInYear)) ]
            , text " weeks."
            ]
        , p []
            [ text "You were born "
            , viewField model "30" "input--day" SetBirthDay
            , text " of "
            , Html.map DropdownMsg (Dropdown.view model.dropdownModel "August" (List.map Tuple.first months))
            , text ", "
            , viewField model "1991" "input--year" SetBirthYear
            , text "."
            ]
        , p [ classList [ ( "count-weeks--visible", model.weeks > 0 ), ( "count-weeks", True ) ] ]
            [ text "You've already lived for "
            , b [] [ text (toString model.weeks) ]
            , text " weeks. Left "
            , b [] [ text (toString <| (model.yearsCount * weekInYear - model.weeks)) ]
            , text " weeks."
            , text "Is it a lot?"
            ]
        , viewCalendar model
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
