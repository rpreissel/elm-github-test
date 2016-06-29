module Github exposing (..)

import Html exposing (Html,ul,li,text,div,form,label,button,input)
import Html.Attributes exposing (value,for,id,type',class)
import Html.Events exposing (onClick, onInput)
import Html.App as Html
import Http
import Json.Decode as Json exposing ((:=))
import Task

main =
    Html.program
        { init = init "rpreissel"
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type alias Model =
    { user : String,
      repos : List String
    }

type Msg
  = FetchReposSucceed (List String)
  | FetchRepos
  | UserNameChanged String
  | FetchFail Http.Error

init : String -> (Model, Cmd Msg)
init name =
  ({user = name, repos = []}, fetchReposFromUser name)

view : Model -> Html Msg
view model =
  div [class "container"] [
    div [class "row"] [
      div [class "form-horizontal col-md-4"] [
        div [class "form-group"] [
          label [ for "username-field" ] [ text "Username" ],
          input [ class "form-control", id "username-field", type' "text", value model.user, onInput UserNameChanged] []
        ],
        div [class "form-group"] [
          button [ class "btn btn-primary", onClick FetchRepos] [ text "Fetch Repos" ]
        ]
      ]
    ],
    div [class "row"] [
      text "Repos"
    ],
    div [class "row"] [
      ul []
        (List.map (\r -> li [] [ text r ]) model.repos)
    ]
  ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FetchReposSucceed repos
      -> Debug.log(toString repos)
         ({model | repos = repos}, Cmd.none)
    FetchFail error
      -> Debug.log(toString error)
         ({model | repos = []}, Cmd.none)
    UserNameChanged user
      -> Debug.log(user)
         ({model | user = user}, Cmd.none)
    FetchRepos
      -> (model, fetchReposFromUser model.user)


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

fetchReposFromUser : String -> Cmd Msg
fetchReposFromUser user =
  let
    url =
      "https://api.github.com/users/" ++ user ++ "/repos"
  in
    Task.perform FetchFail FetchReposSucceed (Http.get decodeRepos url)

decodeRepos : Json.Decoder (List String)
decodeRepos =
  Json.list ("name" := Json.string)
