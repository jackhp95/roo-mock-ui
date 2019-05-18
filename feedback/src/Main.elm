module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events as E exposing (..)
import Task



---- MODEL ----


type alias Model =
    { formTitle : String
    , actionEndpoint : String
    , rating : Maybe String
    , feedback : Maybe String
    , aside : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { formTitle = "feedback"
      , actionEndpoint = "https://hooks.zapier.com/hooks/catch/1843357/jns0oj/"
      , rating = Nothing
      , feedback = Nothing
      , aside = 0
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = OnRating String
    | OnFeedback String
    | OnSubmit
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        focusFeedbackTextarea =
            Task.attempt (always NoOp) (Dom.focus "feedback-textarea")
    in
    case msg of
        OnRating val ->
            ( { model | rating = Just val }
            , focusFeedbackTextarea
            )

        OnFeedback val ->
            ( { model | feedback = Just val }, Cmd.none )

        OnSubmit ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        ifRating then_ else_ =
            case model.rating of
                Nothing ->
                    else_

                Just _ ->
                    then_
    in
    div [ class "fixed absolute--fill bg-black-70 z-max flex flex-column items-center justify-center" ]
        [ main_
            [ class "fadeInUp animated faster measure-wide-ns flex flex-wrap-ns flex-column flex-row-ns ma3-ns shadow-5 bg-white overflow-hidden-ns overflow-auto br5"

            -- , class <| ifRating "br5-ns flex-auto" "br5"
            -- , style "min-width" <| ifRating "100%" "70%"
            , style "transition-duration" "1s"
            , style "transition-property" "all"
            ]
            [ asideView model
            , Html.form
                [ id model.formTitle
                , action model.actionEndpoint
                , method "POST"
                , class "pb3 flex-grow-1-ns z-1 relative bg-white shadow-4"
                ]
                [ navBar model
                , ratingList model
                , convertView model
                , feedbackTextarea model
                , submitMenu model
                ]
            ]
        ]


type Cardinality
    = To
    | From


asideView : Model -> Html Msg
asideView model =
    case model.aside of
        0 ->
            text ""

        1 ->
            header [ class "self-stretch flex-auto flex flex-wrap" ]
                [ section [ class "mw5 flex-auto pa3 flex flex-column justify-start" ]
                    [ messageView From "Can I include that my space is climate controlled?"
                    , span [ class "o-80 pa3" ] [ text "Eric wanted spaces to be labeled if they were climate controlled." ]
                    ]
                , section [ class "mw5 flex-auto pa3 flex flex-column justify-end" ]
                    [ span [ class "o-80 pa3" ] [ text "So Joe added climate control to our feature list!" ]
                    , messageView To "Great Idea!"
                    ]
                ]

        _ ->
            header [ class "self-stretch flex-auto flex flex-wrap img-child" ]
                [ img
                    [ src "https://images.unsplash.com/photo-1456406644174-8ddd4cd52a06"
                    , alt "Working on Roo"
                    ]
                    []
                , section [ class "mw5 flex-auto pa3 flex flex-column justify-start" ]
                    []
                ]


messageView : Cardinality -> String -> Html Msg
messageView cardin txt =
    let
        toOrFrom to from =
            case cardin of
                To ->
                    to

                From ->
                    from
    in
    div [ class "relative flex" ]
        [ article
            [ class "z-1 relative overflow-hidden shadow-3 dib mw5-ns br5 pa1 fw5 lh-title"
            , class <| toOrFrom "ml-auto bg-blue white" "mr-auto bg-orange white"
            ]
            [ div
                [ class "parallax absolute absolute--fill lg-white-fade-down o-30"
                , style "mix-blend-mode" "soft-light"
                ]
                []
            , p [ class "relative lh-title measure ma2 tracked-1" ] [ text txt ]
            ]
        , div
            [ class "bw4 absolute bottom-0"
            , class <| toOrFrom "bl b--blue right--1" "br b--orange left--1"
            , (\x -> style ("border-bottom-" ++ x ++ "-radius") "1rem") <| toOrFrom "left" "right"
            , style "height" "1rem"
            , style "width" "1.5rem"
            ]
            []
        ]


navBar : Model -> Html Msg
navBar model =
    nav [ class "lh-solid pa3 flex flex-wrap-reverse justify-between bb b--black-10" ]
        [ h1 [ class "slideInLeft animated blue" ] [ text "Feedback" ]
        , button [ class "slideInRight animated pa0 bn h-auto w-auto" ]
            [ img
                [ src "https://icongr.am/clarity/close.svg"
                , alt "close"
                , class "db"
                , style "height" "1em"
                , style "width" "1em"
                ]
                []
            ]
        ]


ratingList : Model -> Html Msg
ratingList model =
    let
        condAttrs li givenAttrs =
            givenAttrs
                ++ (case model.rating of
                        Nothing ->
                            [ class "o-100" ]

                        Just val ->
                            case val == li of
                                True ->
                                    [ class "o-100"
                                    , style "transform" "scale(1)"
                                    ]

                                False ->
                                    [ class "o-40"
                                    , style "transform" "scale(0.62)"
                                    ]
                   )

        liEmoji rating =
            li (condAttrs rating [ class "flex-auto grow-large relative" ])
                [ label
                    [ for <| "select" ++ rating
                    , class "absolute absolute--fill flex items-center justify-center"
                    ]
                    [ text rating ]
                , input
                    [ class "o-0 h-auto w-auto pt0 pl5 ma0 bn aspect-ratio--4x3-ns aspect-ratio--1x1"
                    , type_ "radio"
                    , id <| "select" ++ rating
                    , Attr.value <| rating
                    , name "ratingList"
                    , E.onInput OnRating
                    ]
                    []
                ]

        ifRating then_ else_ =
            case model.rating of
                Nothing ->
                    else_

                Just _ ->
                    then_
    in
    section [ class "mh3 pt3" ]
        [ h2 [ class "fadeInDown slow animated f-1 lh-copy tc navy" ] [ text "How are you feeling?" ]
        , List.map liEmoji
            [ "😍"
            , "🙂"
            , "🙁"
            , "😭"
            ]
            |> ol [ class "animated bounceIn flex tc f5 lh-solid"
            , class <| ifRating "mt0" "mt3" ]
        ]


convertView : Model -> Html Msg
convertView model =
    case model.rating of
        Just _ ->
            aside [ class "fadeIn animated pa3 bg-black-05 flex items-center lh-copy mb3" ]
                [ figure [ class "flex-none" ]
                    [ img [ class "br-100 overflow-hidden h3 w3 db", src "https://images.unsplash.com/photo-1456406644174-8ddd4cd52a06", alt "Jack" ] []
                    , figcaption [ class "tc blue nb2 mt1" ] [ text "Jack" ]
                    ]
                , p [ class "ml3 mw5 o-80" ] [ text "Hello, I'm Jack. I work on making Roo as nice as it can be. I'd love to hear your suggestions on how to improve roo! Thank you!" ]
                ]

        Nothing ->
            text ""


feedbackTextarea : Model -> Html Msg
feedbackTextarea model =
    let
        ifRating then_ else_ =
            case model.rating of
                Nothing ->
                    else_

                Just _ ->
                    then_

        ifFeedback then_ else_ =
            case model.feedback of
                Nothing ->
                    else_

                Just _ ->
                    then_
    in
    div
        ([ class "fadeIn animated delay-1s overflow-hidden lh-copy" ]
            ++ ifRating [ class "ph3 pb3"] [ class "dn"]
        )
        [ label [ class "f-1 mh2 navy", for "feedback-textarea" ] [ text "Suggestions" ]
        , textarea
            [ id "feedback-textarea"
            , name "feedback-textarea"
            , E.onInput OnFeedback
            , class "self-center lh-copy"
            ]
            [ text <| Maybe.withDefault "" model.feedback ]
        ]


submitMenu : Model -> Html Msg
submitMenu model =
    case ( model.rating /= Nothing, model.feedback /= Nothing ) of
        ( True, True ) ->
            menu [ class "fadeIn animated delay-2s ph3 flex justify-between items-center i" ]
                [ span [ class "ph2 o-50" ] [ text "Thank you!" ]
                , button [ class "bg-blue hover-bg-navy white" ] [ text "submit" ]
                ]

        ( True, False ) ->
            menu [ class "fadeIn animated delay-2s ph3 flex justify-end" ]
                [ button [ class "bg-blue hover-bg-navy white db" ] [ text "submit" ]
                ]

        _ ->
            text ""



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
