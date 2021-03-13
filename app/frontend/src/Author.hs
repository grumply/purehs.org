module Author (author) where

import App (req,App)
import qualified Components.Avatar as Avatar
import qualified Components.GitHubName as GitHubName
import qualified Components.Email as Email
import qualified Components.Markdown as Markdown
import qualified Components.Title as Title
import qualified Components.Twitter as Twitter
import qualified Components.Company as Company
import qualified Components.Problem as Problem
import Data.Route
import Styles.Themes

import Shared (backend,getAuthor,getAuthorContent)
import Shared.Author (Author(..),AuthorContent(..))
import Shared.Types (Name)

import Pure.Elm.Application
import Pure.Maybe (consuming, producingKeyed)

import Control.Monad (when)
import Data.Maybe (isNothing)

author :: App.App => Name -> View
author a = producingKeyed a producer (\_ -> consuming consumer)
  where
    producer a = do
      ma  <- App.req backend getAuthor a
      mac <- App.req backend getAuthorContent a
      when (isNothing ma) do
        retitle "Not Found"
      pure do
        a  <- ma
        ac <- mac
        pure (a,ac)

    consumer Nothing = Problem.notFound "Author"
    consumer (Just (Author {..},AuthorContent md)) = 
      Div <| Themed @Load |>
        [ Article <| Themed @Article |> 
          [ Header <| Themed @Header |>
            [ Avatar.avatars [name]
            , Title.title (AuthorRoute (AuthorR name)) (toTxt name)
            , maybe Null GitHubName.gitHubNameLink github
            , maybe Null Twitter.twitterHandleLink twitter
            , maybe Null Email.emailLink email
            , maybe Null Company.companyLink company
            ]
          , Markdown.markdown md
          ]
        ]