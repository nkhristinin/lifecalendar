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


open =
    SetOpen True


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


view : Model -> String -> List String -> Html Msg
view model defaultText values  =
    div
        [ class "dropdown"
        , tabindex 0
        , onBlur close
        , onClick open
        ]
        [ div [ class "dropdown__selected-text" ]
            [ text (model.selectedValue |> Maybe.withDefault defaultText) ]
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
