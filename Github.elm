module Github exposing (..)

import Html exposing (Html,ul,li,text,div,form,label,button,input)
import Html.Attributes exposing (value,for,id,type_,class)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JD exposing (..)
import Task
import GithubApiToken exposing (apiToken)

main =
    Html.program
        { init = init "rpreissel"
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type alias Model =
    { user : String,
      githubApiToken : String,
      repos : List String
    }

type Msg
  = ReposFetched (Result Http.Error (List String))
  | SampleQueryFetched (Result Http.Error String)
  | FetchRepos
  | TestNewApi
  | UserNameChanged String

init : String -> (Model, Cmd Msg)
init name =
  ({user = name, githubApiToken = apiToken, repos = []}, fetchReposFromUser name)

view : Model -> Html Msg
view model =
  div [class "container"] [
    div [class "row"] [
      div [class "form-horizontal col-md-4"] [
        div [class "form-group"] [
          label [ for "username-field" ] [ text "Username" ],
          input [ class "form-control", id "username-field", type_ "text", Html.Attributes.value model.user, onInput UserNameChanged] [],
          label [ for "git-hub-api-token" ] [ text "Github API Token" ],
          input [ class "form-control", id "git-hub-api-token", type_ "password", Html.Attributes.value model.githubApiToken] []
        ],
        div [class "form-group"] [
          button [ class "btn btn-primary", onClick FetchRepos] [ text "Fetch Repos" ],
          button [ class "btn btn-primary", onClick TestNewApi] [ text "Test new API" ]
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
    ReposFetched (Result.Ok rrepos)
      -> ({model | repos = rrepos}, Cmd.none)
    ReposFetched (Result.Err message)
      -> ({model | repos = [ toString message ]}, Cmd.none)
    SampleQueryFetched (Result.Ok rstring)
      -> ({model | repos = [ "new api", rstring ]}, Cmd.none)
    SampleQueryFetched (Result.Err message)
      -> ({model | repos = [ toString message ]}, Cmd.none)
    UserNameChanged user
      -> ({model | user = user}, Cmd.none)
    FetchRepos
      -> (model, fetchReposFromUser model.user)
    TestNewApi
      -> (model, callNewApi model.githubApiToken)


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

fetchReposFromUser : String -> Cmd Msg
fetchReposFromUser user =
  let
    url =
      "https://api.github.com/users/" ++ user ++ "/repos"
  in
    Http.send ReposFetched <|
      (Http.get url decodeRepos )


callNewApi : String -> Cmd Msg
callNewApi user =
    let
      rq = Http.request
        { method = "POST"
        , headers = [ Http.header  "Authorization" "bearer 03e3a5e6ce23055a3e53852e177d0ab5684c8c23" ]
        , url = "https://api.github.com/graphql"
        , body = Http.stringBody "application/json" "{ \"query\": \"query { viewer { login }}\""
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
      }
    in
      Http.send SampleQueryFetched rq


decodeRepos : JD.Decoder (List String)
decodeRepos =
  JD.list (JD.field "name" JD.string)
