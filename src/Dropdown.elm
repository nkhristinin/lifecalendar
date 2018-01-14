module Dropdown exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json


---- MODEL ----


type alias DropdownItem =
    Maybe String


type alias Model =
    { isOpen : Bool
    , selectedValue : DropdownItem
    }


initialModel : Model
initialModel =
    ({ isOpen = False
     , selectedValue = Nothing
     }
    )



---- UPDATE -----

open: Msg
open =
    SetOpen True

close: Msg
close =
    SetOpen False


type Msg
    = SelectItem DropdownItem
    | SetOpen Bool


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectItem item ->
            ({ model | selectedValue = item, isOpen = False })

        SetOpen isOpen ->
            ({ model | isOpen = isOpen })



---- VIEW -----


viewDropdownItem : String -> Html Msg
viewDropdownItem item =
    li [ class "dropdown__item", onClickItem <| SelectItem <| Just item ]
        [ text item ]

viewSelectedText: Model -> String-> Html Msg
viewSelectedText model defaultText =
    let isSelected =  
        case model.selectedValue of
            Just title -> True
            Nothing -> False
    in
             div [ class "dropdown__title", classList [("dropdown__title--selected", isSelected)] ]
            [ text (model.selectedValue |> Maybe.withDefault defaultText) ]

view : Model -> String -> List String -> Html Msg
view model defaultText values  =
    div
        [ class "dropdown"
        , tabindex 0
        , onBlur close
        , onFocus open
        , onClick open
        ]
        [ viewSelectedText model defaultText
        , ul [ class "dropdown__panel", classList [ ( "dropdown__panel--visible", model.isOpen ) ] ]
            (List.map viewDropdownItem values)
        ]


onClickItem : msg -> Attribute msg
onClickItem message =
    onWithOptions
        "click"
        { stopPropagation = True
        , preventDefault = False
        }
        (Json.succeed message)
