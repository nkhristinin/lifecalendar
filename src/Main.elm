module Main exposing (..)

import Date exposing (Date)
import Task
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Dropdown


---- MODEL ----


type alias Week =
    { index : Int
    , filled : Bool
    }


type alias MonthInfo =
    { title : String
    , month : Date.Month
    , number : Int
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


weekList : Int -> List Week
weekList year =
    List.map (\x -> Week x False)
        (List.range 1 (year * weekInYear))


weekInYear : Int
weekInYear =
    52


months : List MonthInfo
months =
    [ { title = "January"
      , month = Date.Jan
      , number = 1
      }
    , { title = "February"
      , month = Date.Feb
      , number = 2
      }
    , { title = "March"
      , month = Date.Mar
      , number = 3
      }
    , { title = "April"
      , month = Date.Apr
      , number = 4
      }
    , { title = "May"
      , month = Date.May
      , number = 5
      }
    , { title = "June"
      , month = Date.Jun
      , number = 6
      }
    , { title = "July"
      , month = Date.Jul
      , number = 7
      }
    , { title = "August"
      , month = Date.Aug
      , number = 8
      }
    , { title = "September"
      , month = Date.Sep
      , number = 9
      }
    , { title = "October"
      , month = Date.Oct
      , number = 10
      }
    , { title = "November"
      , month = Date.Nov
      , number = 11
      }
    , { title = "December"
      , month = Date.Dec
      , number = 12
      }
    ]


findMonthBy : (MonthInfo -> a) -> a -> Maybe MonthInfo
findMonthBy access param =
    months
        |> List.filter (\m -> (access m) == param)
        |> List.head


nowMonthNumber : Date -> Int
nowMonthNumber now =
    now
        |> Date.month
        |> findMonthBy .month
        |> Maybe.map .number
        |> Maybe.withDefault 0


split : Int -> List a -> List (List a)
split i list =
    case List.take i list of
        [] ->
            []

        listHead ->
            listHead :: split i (List.drop i list)


countOfWeeks : Model -> Int
countOfWeeks model =
    model.yearsCount * weekInYear


weeksLeft : Model -> Int
weeksLeft model =
    (countOfWeeks model) - model.weeks



---- UPDATE ----


type Msg
    = GetDateNow Date
    | SetBirthYear String
    | SetBirthMonth String
    | SetBirthDay String
    | ShowCalendar
    | DropdownMsg Dropdown.Msg


toInt : String -> Maybe Int
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
            ((nowMonthNumber now) - month - 1) * 4

        dayWeeks =
            ((Date.day now) - day) // 7
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
                    , birthMonth =
                        dropdownModel.selectedValue
                            |> Maybe.withDefault ""
                            |> findMonthBy .title
                            |> Maybe.map .number
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
            |> List.append [ viewDirections ]
        )


viewDirections : Html Msg
viewDirections =
    div [ class "directions" ]
        [ div [ class "direction direction__week" ] [ text "Weeks" ]
        ]


viewFooter : Html Msg
viewFooter =
    footer []
        [ p []
            [ text "Using "
            , a [ href "http://elm-lang.org/", rel "noopener", target "_blank" ]
                [ text "Elm" ]
            ]
        , p []
            [ a [ href "https://github.com/hrnik/lifecalendar", rel "noopener", target "_blank" ]
                [ text "Source code" ]
            , text " on GitHub"
            ]
        , p []
            [ text "Inspired by this "
            , a [ href "https://medium.com/design-productivity/%D0%BA%D0%B0%D0%BB%D0%B5%D0%BD%D0%B4%D0%B0%D1%80%D1%8C-%D0%B6%D0%B8%D0%B7%D0%BD%D0%B8-fac1327d676c", rel "noopener", target "_blank" ]
                [ text "article" ]
            ]
        , p []
            [ text "Icons from "
            , a [ href "https://icons8.com", rel "noopener", target "_blank" ]
                [ text "Icons8.com" ]
            ]
        ]


viewDropDown : Model -> Html Msg
viewDropDown model =
    Html.map DropdownMsg (Dropdown.view model.dropdownModel "August" (List.map .title months))


view : Model -> Html Msg
view model =
    div []
        [ p []
            [ text ("Imagine that the average life expectancy is ")
            , b [] [ text (toString model.yearsCount) ]
            , text (" years. This is approximately ")
            , b [] [ text (toString (countOfWeeks model)) ]
            , text " weeks."
            ]
        , p []
            [ text "You were born "
            , viewField model "30" "input--day" SetBirthDay
            , text " of "
            , viewDropDown model
            , text ", "
            , viewField model "1991" "input--year" SetBirthYear
            , text "."
            ]
        , p [ classList [ ( "count-weeks--visible", model.weeks > 0 ), ( "count-weeks", True ) ] ]
            [ text "You've already lived for "
            , b [] [ text (toString model.weeks) ]
            , text " weeks. Left "
            , b [] [ text (toString <| weeksLeft <| model) ]
            , text " weeks."
            , text "Is it a lot?"
            ]
        , viewCalendar model
        , viewFooter
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
