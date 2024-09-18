module Main exposing (main)

import Browser
import Element exposing (Element, column, fill, height, padding, paragraph, row, spacing, width)
import Element.Background
import Element.Border
import Element.Font as Font
import Element.Input
import Html exposing (Html)
import Html.Attributes
import Markdown.Block exposing (ListItem(..), Task(..))
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer


type alias Model =
    String


type Msg
    = OnMarkdownChange String


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init flags =
    ( "", Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnMarkdownChange markdown ->
            ( markdown, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Title of the page"
    , body =
        [ Element.layout [ height fill ]
            (row
                [ width fill
                , height fill
                , spacing 40
                , padding 40
                ]
                [ Element.Input.multiline
                    [ width fill, height fill ]
                    { onChange = OnMarkdownChange
                    , text = model
                    , placeholder = Nothing
                    , label = Element.Input.labelHidden "Markdown Editor"
                    , spellcheck = False
                    }
                , column
                    [ width fill, height fill ]
                    [ case viewMarkdown model of
                        Ok rendered ->
                            Element.column
                                []
                                rendered

                        Err err ->
                            Element.text <| err
                    ]
                ]
            )
        ]
    }


viewMarkdown model =
    model
        |> Markdown.Parser.parse
        |> Result.mapError (List.map Markdown.Parser.deadEndToString >> String.join "\n")
        |> Result.andThen (\ast -> Markdown.Renderer.render renderer ast)


renderer : Markdown.Renderer.Renderer (Element Msg)
renderer =
    elmUiRenderer


elmUiRenderer : Markdown.Renderer.Renderer (Element Msg)
elmUiRenderer =
    { heading = heading
    , paragraph =
        Element.paragraph
            [ Element.spacing 15 ]
    , thematicBreak = Element.none
    , text = Element.text
    , strong = \content -> Element.row [ Font.bold ] content
    , emphasis = \content -> Element.row [ Font.italic ] content
    , strikethrough = \content -> Element.row [ Font.strike ] content
    , codeSpan = code
    , link =
        \{ title, destination } body ->
            Element.newTabLink
                [ Element.htmlAttribute (Html.Attributes.style "display" "inline-flex") ]
                { url = destination
                , label =
                    Element.paragraph
                        [ Font.color (Element.rgb255 0 0 255)
                        ]
                        body
                }
    , hardLineBreak = Html.br [] [] |> Element.html
    , image =
        \image ->
            case image.title of
                Just title ->
                    Element.image [ Element.width Element.fill ] { src = image.src, description = image.alt }

                Nothing ->
                    Element.image [ Element.width Element.fill ] { src = image.src, description = image.alt }
    , blockQuote =
        \children ->
            Element.column
                [ Element.Border.widthEach { top = 0, right = 0, bottom = 0, left = 10 }
                , Element.padding 10
                , Element.Border.color (Element.rgb255 145 145 145)
                , Element.Background.color (Element.rgb255 245 245 245)
                ]
                children
    , unorderedList =
        \items ->
            Element.column [ Element.spacing 15 ]
                (items
                    |> List.map
                        (\(ListItem task children) ->
                            Element.row [ Element.spacing 5 ]
                                [ Element.row
                                    [ Element.alignTop ]
                                    ((case task of
                                        IncompleteTask ->
                                            Element.Input.defaultCheckbox False

                                        CompletedTask ->
                                            Element.Input.defaultCheckbox True

                                        NoTask ->
                                            Element.text "•"
                                     )
                                        :: Element.text " "
                                        :: children
                                    )
                                ]
                        )
                )
    , orderedList =
        \startingIndex items ->
            Element.column [ Element.spacing 15 ]
                (items
                    |> List.indexedMap
                        (\index itemBlocks ->
                            Element.row [ Element.spacing 5 ]
                                [ Element.row [ Element.alignTop ]
                                    (Element.text (String.fromInt (index + startingIndex) ++ " ") :: itemBlocks)
                                ]
                        )
                )
    , codeBlock = codeBlock
    , html = Markdown.Html.oneOf []
    , table = Element.column []
    , tableHeader = Element.column []
    , tableBody = Element.column []
    , tableRow = Element.row []
    , tableHeaderCell =
        \maybeAlignment children ->
            Element.paragraph [] children
    , tableCell =
        \maybeAlignment children ->
            Element.paragraph [] children
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



{-
   #########################################
   ||                                     ||
   ||       Markdown Components           ||
   ||                                     ||
   #########################################
-}


heading :
    { level : Markdown.Block.HeadingLevel
    , rawText : String
    , children : List (Element Msg)
    }
    -> Element Msg
heading { level, rawText, children } =
    Element.column
        [ Font.size
            (case level of
                Markdown.Block.H1 ->
                    30

                Markdown.Block.H2 ->
                    28

                _ ->
                    25
            )
        , Element.paddingEach (cPadding 0 45 0 0)
        ]
        children


code : String -> Element msg
code snippet =
    Element.el
        [ Element.Background.color
            (Element.rgba 0 0 0 0.04)
        , Element.Border.rounded 2
        , Element.paddingXY 5 3
        , Font.family
            [ Font.external
                { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                , name = "Source Code Pro"
                }
            ]
        ]
        (Element.text snippet)


codeBlock : { body : String, language : Maybe String } -> Element msg
codeBlock details =
    Element.el
        [ Element.Background.color (Element.rgba 0 0 0 0.03)
        , Element.htmlAttribute (Html.Attributes.style "white-space" "pre")
        , Element.padding 20
        , Font.family
            [ Font.external
                { url = "https://fonts.googleapis.com/css?family=Source+Code+Pro"
                , name = "Source Code Pro"
                }
            ]
        ]
        (Element.text details.body)


cPadding :
    Int
    -> Int
    -> Int
    -> Int
    -> { top : Int, bottom : Int, left : Int, right : Int }
cPadding top bottom left right =
    { top = top, bottom = bottom, left = left, right = right }
