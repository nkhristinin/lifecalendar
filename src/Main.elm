module Main exposing (..)

import Date exposing (Date)
import Task
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
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


weekInYear : Int
weekInYear =
    52


months : List ( String, Date.Month )
months =
    [ ( "January", Date.Jan )
    , ( "February", Date.Feb )
    , ( "March", Date.Mar )
    , ( "April", Date.Apr )
    , ( "May", Date.May )
    , ( "June", Date.Jun )
    , ( "July", Date.Jul )
    , ( "August", Date.Aug )
    , ( "September", Date.Sep )
    , ( "October", Date.Oct )
    , ( "November", Date.Nov )
    , ( "December", Date.Dec )
    ]

monthForList = months
    |> List.map (\month -> (Tuple.first month, getMonthNow (Tuple.second month)) )
    |> Dict.fromList

getMonthNow month = 
    case month of
        Date.Jan -> 1
        Date.Feb -> 2
        Date.Mar -> 3
        Date.Apr -> 4
        Date.May -> 5
        Date.Jun -> 6
        Date.Jul -> 7
        Date.Aug -> 8
        Date.Sep -> 9
        Date.Oct -> 10
        Date.Nov -> 11
        Date.Dec -> 12
            


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
            ((getMonthNow <| Date.month <| now) - month - 1 ) * 4

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
                    , birthMonth = (Dict.get (dropdownModel.selectedValue |> Maybe.withDefault "") monthForList)
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

viewFooter: Html Msg
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
            , a [ href "(https://medium.com/design-productivity/%D0%BA%D0%B0%D0%BB%D0%B5%D0%BD%D0%B4%D0%B0%D1%80%D1%8C-%D0%B6%D0%B8%D0%B7%D0%BD%D0%B8-fac1327d676c)", rel "noopener", target "_blank" ]
                [ text "article" ]
            ]
        , p []
            [ text "Icons from "
            , a [ href "https://icons8.com", rel "noopener", target "_blank" ]
                [ text "Icons8.com" ]
            ]
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
