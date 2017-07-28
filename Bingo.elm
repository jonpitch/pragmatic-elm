module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Random
import Http
import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Encode as Encode
import ViewHelpers exposing (..)
import Entry

-- model
type GameState = EnteringName | Playing

type alias Model =
  { name : String
  , gameNumber: Int
  , entries : List Entry.Entry
  , alertMessage : Maybe String
  , nameInput : String
  , gameState : GameState
  }
  
type alias Score =
  { id: Int
  , name : String
  , score : Int
  }

initialModel : Model
initialModel =
  {
    name = "Anonymous"
    , gameNumber = 1
    , entries = []
    , alertMessage = Nothing
    , nameInput = ""
    , gameState = EnteringName
  }
  
-- update
type Msg = NewGame 
  | Mark Int 
  | NewRandom Int 
  | NewEntries (Result Http.Error (List Entry.Entry))
  | CloseAlert
  | ShareScore
  | NewScore (Result Http.Error Score)
  | SetNameInput String
  | SaveName
  | CancelName
  | ChangeGameState GameState

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of 
    NewRandom randomNumber ->
      ( { model | gameNumber = randomNumber }, Cmd.none )
      
    ShareScore ->
      ( model, postScore model )
      
    SetNameInput value ->
      ( { model | nameInput = value }, Cmd.none )
      
    ChangeGameState state -> 
      ( { model | gameState = state }, Cmd.none )
      
    SaveName ->
      ( { model | name = model.nameInput
        , nameInput = ""
        , gameState = Playing }, Cmd.none)
    
    CancelName ->
      ( { model | nameInput = ""
        , gameState = Playing }, Cmd.none )

    NewScore result ->
      case result of
        Ok score ->
          let
            message =
              "Your score of "
                ++ (toString score.score)
                ++ " was successfully shared!"
          in
            ( { model | alertMessage = Just message }, Cmd.none )
        Err error ->
          let
            message =
              "Error posting your score: "
                ++ (toString error)
          in
            ( { model | alertMessage = Just message }, Cmd.none )
      
    NewGame ->
      ({ model | gameNumber = model.gameNumber + 1 }, getEntries )
      
    NewEntries result ->
      case result of
        Ok randomEntries ->
          ( { model | entries = List.sortBy .points randomEntries }, Cmd.none )
            
        Err error -> 
            ( { model | alertMessage = Just (httpErrorToMessage error) }, Cmd.none )
            
    CloseAlert ->
      ( { model | alertMessage = Nothing }, Cmd.none )
    
    Mark id ->
      ({ model | entries = Entry.markEntryWithId model.entries id }, Cmd.none)
      
httpErrorToMessage : Http.Error -> String
httpErrorToMessage error =
  case error of
    Http.NetworkError ->
      "server offline"
    Http.Timeout ->
      "Request timed out"
    Http.BadStatus response ->
      case response.status.code of
        401 ->
          "Unauthorized"
        404 ->
          "Not Found"
        code ->
          (toString code)
    Http.BadPayload message _ ->
      "Decode failed: " ++ message
    _ -> 
      (toString error)
        
-- decoders

    
scoreDecoder : Decoder Score
scoreDecoder =
  Decode.map3 Score
    (field "id" Decode.int)
    (field "name" Decode.string)
    (field "score" Decode.int)
    
-- encoders
encodeScore : Model -> Encode.Value
encodeScore model =
  Encode.object
    [ ("name", Encode.string model.name)
    , ("score", Encode.int (Entry.sumMarkedPoints model.entries))
    ]
        
-- commands
generateRandomNumber : Cmd Msg
generateRandomNumber =
  Random.generate NewRandom (Random.int 1 100)
  
apiUrl : String
apiUrl =
  "http://localhost:3000"
  
entriesUrl : String
entriesUrl =
  apiUrl ++ "/random-entries"
  
getEntries : Cmd Msg
getEntries =
  Entry.getEntries NewEntries entriesUrl
    
postScore : Model -> Cmd Msg
postScore model =
  let
    url =
      apiUrl ++ "/scores"
    
    body =
      encodeScore model
        |> Http.jsonBody
    
    request =
      Http.post url body scoreDecoder
  in
    Http.send NewScore request

-- view
viewPlayer : String -> Int -> Html Msg
viewPlayer name gameNumber =
  h2 [ id "info", class "classy" ]
    [ a [ href "#", onClick (ChangeGameState EnteringName) ]
      [ text name ]
    , text (" - Game #" ++ (toString gameNumber))
    ]
  
viewHeader : String -> Html Msg    
viewHeader title =
  header []
    [ h1 [] [ text title ] ]
  
viewFooter : Html Msg  
viewFooter =
  footer []
    [ a 
      [ href "http:://elm-lang.org" ]
      [ text "Powered By Elm"]
    ]
    
viewScore : Int -> Html Msg
viewScore sum =
  div
    [ class "score" ]
    [ span [ class "label" ] [ text "Score" ]
    , span [ class "value" ] [ text (toString sum) ]
    ]
    
hasZeroScore : Model -> Bool
hasZeroScore model =
  (Entry.sumMarkedPoints model.entries) == 0
  
hasNoNameInput : Model -> Bool
hasNoNameInput model =
  String.isEmpty model.nameInput == True

view : Model -> Html Msg
view model =
  div [ class "content" ]
    [ viewHeader "BUZZWORD BINGO"
      , viewPlayer model.name model.gameNumber
      , alert CloseAlert model.alertMessage
      , viewNameInput model
      , Entry.viewEntryList Mark model.entries
      , viewScore (Entry.sumMarkedPoints model.entries)
      , div [ class "button-group" ]
          [ button [ onClick NewGame ] [ text "New Game" ]
          , button [ onClick ShareScore, disabled (hasZeroScore model) ] 
            [ text "Share Score"] 
          ]
      , div [ class "debug" ] [ text (toString model) ]
      , viewFooter
    ]
    
viewNameInput : Model -> Html Msg
viewNameInput model =
  case model.gameState of
    EnteringName ->
      div [ class "name-input" ]
        [ input
          [ type_ "text"
            , placeholder "Who's Playing?"
            , autofocus True
            , value model.nameInput
            , onInput SetNameInput
          ]
          []
          , button [ onClick SaveName, disabled (hasNoNameInput model) ] [ text "Save" ]
          , primaryButton CancelName "Cancel"
        ]
    Playing ->
      text ""

main : Program Never Model Msg
main =  
  program
    { init = ( initialModel, getEntries )
    , view = view
    , update = update
    , subscriptions = (\_ -> Sub.none)
    }
