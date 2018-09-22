module Page.Article exposing (Model, Msg, init, subscriptions, toSession, update, view)

{-| Viewing an individual article.
-}

import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Article exposing (Article, Full, Preview)
import Article.Body exposing (Body)
import Article.Comment as Comment exposing (Comment)
import Article.Slug as Slug exposing (Slug)
import Author exposing (Author(..), FollowedAuthor, UnfollowedAuthor)
import Avatar
import Browser.Navigation as Nav
import CommentId exposing (CommentId)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, disabled, href, id, placeholder, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode
import Loading
import Log
import Page
import Profile exposing (Profile)
import Route
import Session exposing (Session)
import Step exposing (Step)
import Task exposing (Task)
import Time
import Timestamp
import Username exposing (Username)
import Viewer exposing (Viewer)



-- MODEL


type alias Model =
    { session : Session
    , timeZone : Time.Zone
    , errors : List String

    -- Loaded independently from server
    , comments : Status ( CommentText, List Comment )
    , article : Status (Article Full)
    }


type Status a
    = Loading
    | LoadingSlowly
    | Loaded a
    | Failed


type CommentText
    = Editing String
    | Sending String


init : Session -> Slug -> ( Model, Cmd Msg )
init session slug =
    let
        maybeCred =
            Session.cred session
    in
    ( { session = session
      , timeZone = Time.utc
      , errors = []
      , comments = Loading
      , article = Loading
      }
    , Cmd.batch
        [ Article.fetch maybeCred slug
            |> Http.send CompletedLoadArticle
        , Comment.list maybeCred slug
            |> Http.send CompletedLoadComments
        , Task.perform GotTimeZone Time.here
        , Task.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
        ]
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    case model.article of
        Loaded article ->
            let
                { title } =
                    Article.metadata article

                author =
                    Article.author article

                avatar =
                    Profile.avatar (Author.profile author)

                slug =
                    Article.slug article

                profile =
                    Author.profile author

                buttons =
                    case Session.cred model.session of
                        Just cred ->
                            viewButtons cred article author

                        Nothing ->
                            []
            in
            { title = title
            , content =
                div [ class "article-page" ]
                    [ div [ class "banner" ]
                        [ div [ class "container" ]
                            [ h1 [] [ text title ]
                            , div [ class "article-meta" ] <|
                                List.append
                                    [ a [ Route.href (Route.Profile (Author.username author)) ]
                                        [ img [ Avatar.src (Profile.avatar profile) ] [] ]
                                    , div [ class "info" ]
                                        [ Author.view (Author.username author)
                                        , Timestamp.view model.timeZone (Article.metadata article).createdAt
                                        ]
                                    ]
                                    buttons
                            , Page.viewErrors ClickedDismissErrors model.errors
                            ]
                        ]
                    , div [ class "container page" ]
                        [ div [ class "row article-content" ]
                            [ div [ class "col-md-12" ]
                                [ Article.Body.toHtml (Article.body article) [] ]
                            ]
                        , hr [] []
                        , div [ class "article-actions" ]
                            [ div [ class "article-meta" ] <|
                                List.append
                                    [ a [ Route.href (Route.Profile (Author.username author)) ]
                                        [ img [ Avatar.src avatar ] [] ]
                                    , div [ class "info" ]
                                        [ Author.view (Author.username author)
                                        , Timestamp.view model.timeZone (Article.metadata article).createdAt
                                        ]
                                    ]
                                    buttons
                            ]
                        , div [ class "row" ]
                            [ div [ class "col-xs-12 col-md-8 offset-md-2" ] <|
                                -- Don't render the comments until the article has loaded!
                                case model.comments of
                                    Loading ->
                                        []

                                    LoadingSlowly ->
                                        [ Loading.icon ]

                                    Loaded ( commentText, comments ) ->
                                        -- Don't let users add comments until they can
                                        -- see the existing comments! Otherwise you
                                        -- may be about to repeat something that's
                                        -- already been said.
                                        viewAddComment slug commentText (Session.viewer model.session)
                                            :: List.map (viewComment model.timeZone slug) comments

                                    Failed ->
                                        [ Loading.error "comments" ]
                            ]
                        ]
                    ]
            }

        Loading ->
            { title = "Article", content = text "" }

        LoadingSlowly ->
            { title = "Article", content = Loading.icon }

        Failed ->
            { title = "Article", content = Loading.error "article" }


viewAddComment : Slug -> CommentText -> Maybe Viewer -> Html Msg
viewAddComment slug commentText maybeViewer =
    case maybeViewer of
        Just viewer ->
            let
                avatar =
                    Viewer.avatar viewer

                cred =
                    Viewer.cred viewer

                ( commentStr, buttonAttrs ) =
                    case commentText of
                        Editing str ->
                            ( str, [] )

                        Sending str ->
                            ( str, [ disabled True ] )
            in
            Html.form [ class "card comment-form", onSubmit (ClickedPostComment cred slug) ]
                [ div [ class "card-block" ]
                    [ textarea
                        [ class "form-control"
                        , placeholder "Write a comment..."
                        , attribute "rows" "3"
                        , onInput EnteredCommentText
                        , value commentStr
                        ]
                        []
                    ]
                , div [ class "card-footer" ]
                    [ img [ class "comment-author-img", Avatar.src avatar ] []
                    , button
                        (class "btn btn-sm btn-primary" :: buttonAttrs)
                        [ text "Post Comment" ]
                    ]
                ]

        Nothing ->
            p []
                [ a [ Route.href Route.Login ] [ text "Sign in" ]
                , text " or "
                , a [ Route.href Route.Register ] [ text "sign up" ]
                , text " to comment."
                ]


viewButtons : Cred -> Article Full -> Author -> List (Html Msg)
viewButtons cred article author =
    case author of
        IsFollowing followedAuthor ->
            [ Author.unfollowButton ClickedUnfollow cred followedAuthor
            , text " "
            , favoriteButton cred article
            ]

        IsNotFollowing unfollowedAuthor ->
            [ Author.followButton ClickedFollow cred unfollowedAuthor
            , text " "
            , favoriteButton cred article
            ]

        IsViewer _ _ ->
            [ editButton article
            , text " "
            , deleteButton cred article
            ]


viewComment : Time.Zone -> Slug -> Comment -> Html Msg
viewComment timeZone slug comment =
    let
        author =
            Comment.author comment

        profile =
            Author.profile author

        authorUsername =
            Author.username author

        deleteCommentButton =
            case author of
                IsViewer cred _ ->
                    let
                        msg =
                            ClickedDeleteComment cred slug (Comment.id comment)
                    in
                    span
                        [ class "mod-options"
                        , onClick msg
                        ]
                        [ i [ class "ion-trash-a" ] [] ]

                _ ->
                    -- You can't delete other peoples' comments!
                    text ""

        timestamp =
            Timestamp.format timeZone (Comment.createdAt comment)
    in
    div [ class "card" ]
        [ div [ class "card-block" ]
            [ p [ class "card-text" ] [ text (Comment.body comment) ] ]
        , div [ class "card-footer" ]
            [ a [ class "comment-author", href "" ]
                [ img [ class "comment-author-img", Avatar.src (Profile.avatar profile) ] []
                , text " "
                ]
            , text " "
            , a [ class "comment-author", Route.href (Route.Profile authorUsername) ]
                [ text (Username.toString authorUsername) ]
            , span [ class "date-posted" ] [ text timestamp ]
            , deleteCommentButton
            ]
        ]



-- UPDATE


type Msg
    = ClickedDeleteArticle Cred Slug
    | ClickedDeleteComment Cred Slug CommentId
    | ClickedDismissErrors
    | ClickedFavorite Cred Slug Body
    | ClickedUnfavorite Cred Slug Body
    | ClickedFollow Cred UnfollowedAuthor
    | ClickedUnfollow Cred FollowedAuthor
    | ClickedPostComment Cred Slug
    | EnteredCommentText String
    | CompletedLoadArticle (Result Http.Error (Article Full))
    | CompletedLoadComments (Result Http.Error (List Comment))
    | CompletedDeleteArticle (Result Http.Error ())
    | CompletedDeleteComment CommentId (Result Http.Error ())
    | CompletedFavoriteChange (Result Http.Error (Article Full))
    | CompletedFollowChange (Result Http.Error Author)
    | CompletedPostComment (Result Http.Error Comment)
    | GotTimeZone Time.Zone
    | GotSession Session
    | PassedSlowLoadThreshold


update : Msg -> Model -> Step Model Msg a
update msg model =
    case msg of
        ClickedDismissErrors ->
            Step.to { model | errors = [] }

        ClickedFavorite cred slug body ->
            Step.to model |> Step.command (fave Article.favorite cred slug body)

        ClickedUnfavorite cred slug body ->
            Step.to model |> Step.command (fave Article.unfavorite cred slug body)

        CompletedLoadArticle (Ok article) ->
            Step.to { model | article = Loaded article }

        CompletedLoadArticle (Err error) ->
            Step.to { model | article = Failed }
                |> Step.command Log.error

        CompletedLoadComments (Ok comments) ->
            Step.to { model | comments = Loaded ( Editing "", comments ) }

        CompletedLoadComments (Err error) ->
            Step.to { model | article = Failed }
                |> Step.command Log.error

        CompletedFavoriteChange (Ok newArticle) ->
            Step.to { model | article = Loaded newArticle }

        CompletedFavoriteChange (Err error) ->
            Step.to { model | errors = Api.addServerError model.errors }
                |> Step.command Log.error

        ClickedUnfollow cred followedAuthor ->
            Step.to model
                |> Step.command
                    (Author.requestUnfollow followedAuthor cred
                        |> Http.send CompletedFollowChange
                    )

        ClickedFollow cred unfollowedAuthor ->
            Step.to model
                |> Step.command
                    (Author.requestFollow unfollowedAuthor cred
                        |> Http.send CompletedFollowChange
                    )

        CompletedFollowChange (Ok newAuthor) ->
            case model.article of
                Loaded article ->
                    Step.to { model | article = Loaded (Article.mapAuthor (\_ -> newAuthor) article) }

                _ ->
                    Step.to model
                        |> Step.command Log.error

        CompletedFollowChange (Err error) ->
            Step.to { model | errors = Api.addServerError model.errors }
                |> Step.command Log.error

        EnteredCommentText str ->
            case model.comments of
                Loaded ( Editing _, comments ) ->
                    -- You can only edit comment text once comments have loaded
                    -- successfully, and when the comment is not currently
                    -- being submitted.
                    Step.to { model | comments = Loaded ( Editing str, comments ) }

                _ ->
                    Step.to model |> Step.command Log.error

        ClickedPostComment cred slug ->
            case model.comments of
                Loaded ( Editing "", comments ) ->
                    -- No posting empty comments!
                    -- We don't use Log.error here because this isn't an error,
                    -- it just doesn't do anything.
                    Step.stay

                Loaded ( Editing str, comments ) ->
                    Step.to { model | comments = Loaded ( Sending str, comments ) }
                        |> Step.command
                            (cred
                                |> Comment.post slug str
                                |> Http.send CompletedPostComment
                            )

                _ ->
                    -- Either we have no comment to post, or there's already
                    -- one in the process of being posted, or we don't have
                    -- a valid article, in which case how did we post this?
                    Step.to model |> Step.command Log.error

        CompletedPostComment (Ok comment) ->
            case model.comments of
                Loaded ( _, comments ) ->
                    Step.to { model | comments = Loaded ( Editing "", comment :: comments ) }

                _ ->
                    Step.to model |> Step.command Log.error

        CompletedPostComment (Err error) ->
            Step.to { model | errors = Api.addServerError model.errors } |> Step.command Log.error

        ClickedDeleteComment cred slug id ->
            Step.to model
                |> Step.command
                    (cred
                        |> Comment.delete slug id
                        |> Http.send (CompletedDeleteComment id)
                    )

        CompletedDeleteComment id (Ok ()) ->
            case model.comments of
                Loaded ( commentText, comments ) ->
                    Step.to { model | comments = Loaded ( commentText, withoutComment id comments ) }

                _ ->
                    Step.to model |> Step.command Log.error

        CompletedDeleteComment id (Err error) ->
            Step.to { model | errors = Api.addServerError model.errors }
                |> Step.command Log.error

        ClickedDeleteArticle cred slug ->
            Step.to model
                |> Step.command
                    (delete slug cred
                        |> Http.send CompletedDeleteArticle
                    )

        CompletedDeleteArticle (Ok ()) ->
            Step.to model |> Step.command (Route.replaceUrl (Session.navKey model.session) Route.Home)

        CompletedDeleteArticle (Err error) ->
            Step.to { model | errors = Api.addServerError model.errors }
                |> Step.command Log.error

        GotTimeZone tz ->
            Step.to { model | timeZone = tz }

        GotSession session ->
            Step.to { model | session = session }
                |> Step.command
                    (Route.replaceUrl (Session.navKey session) Route.Home)

        PassedSlowLoadThreshold ->
            let
                -- If any data is still Loading, change it to LoadingSlowly
                -- so `view` knows to render a spinner.
                article =
                    case model.article of
                        Loading ->
                            LoadingSlowly

                        other ->
                            other

                comments =
                    case model.comments of
                        Loading ->
                            LoadingSlowly

                        other ->
                            other
            in
            Step.to { model | article = article, comments = comments }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- HTTP


delete : Slug -> Cred -> Http.Request ()
delete slug cred =
    Api.delete (Endpoint.article slug) cred Http.emptyBody (Decode.succeed ())



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session



-- INTERNAL


fave : (Slug -> Cred -> Http.Request (Article Preview)) -> Cred -> Slug -> Body -> Cmd Msg
fave toRequest cred slug body =
    toRequest slug cred
        |> Http.toTask
        |> Task.map (Article.fromPreview body)
        |> Task.attempt CompletedFavoriteChange


withoutComment : CommentId -> List Comment -> List Comment
withoutComment id list =
    List.filter (\comment -> Comment.id comment /= id) list


favoriteButton : Cred -> Article Full -> Html Msg
favoriteButton cred article =
    let
        { favoritesCount, favorited } =
            Article.metadata article

        slug =
            Article.slug article

        body =
            Article.body article

        kids =
            [ text (" Favorite Article (" ++ String.fromInt favoritesCount ++ ")") ]
    in
    if favorited then
        Article.unfavoriteButton cred (ClickedUnfavorite cred slug body) [] kids

    else
        Article.favoriteButton cred (ClickedFavorite cred slug body) [] kids


deleteButton : Cred -> Article a -> Html Msg
deleteButton cred article =
    let
        msg =
            ClickedDeleteArticle cred (Article.slug article)
    in
    button [ class "btn btn-outline-danger btn-sm", onClick msg ]
        [ i [ class "ion-trash-a" ] [], text " Delete Article" ]


editButton : Article a -> Html Msg
editButton article =
    a [ class "btn btn-outline-secondary btn-sm", Route.href (Route.EditArticle (Article.slug article)) ]
        [ i [ class "ion-edit" ] [], text " Edit Article" ]
