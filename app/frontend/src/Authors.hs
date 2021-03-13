module Authors (authors) where

import App (req,App)
import qualified Components.Avatar as Avatar
import qualified Components.Company as Company
import qualified Components.Email as Email
import qualified Components.Twitter as Twitter
import qualified Components.Title as Title
import qualified Components.GitHubName as GitHubName
import qualified Components.Searcher as Searcher
import Data.Route
import Styles.Themes

import Shared
import Shared.Author

import Pure.Elm.Application
import Pure.Maybe (consuming, producing)

authors :: App.App => View
authors = producing producer (consuming consumer)
  where 
    producer = App.req backend listAuthors ()
      
    consumer = Searcher.searcher listing
      where
        listing v search as = 
          Div <| Themed @Searcher |>
            [ Input <| Value v . OnInput (withInput search) . Placeholder "Search Authors"
            , Div <||> 
              [ Div <| Themed @Listing . Themed @Load |>
                [ Article <| Themed @Article |> 
                  [ Header <| Themed @Header |> 
                    [ Avatar.avatars [name]
                    , Title.title (AuthorRoute (AuthorR name)) (toTxt name)
                    , maybe Null GitHubName.gitHubNameLink github
                    , maybe Null Twitter.twitterHandleLink twitter
                    , maybe Null Email.emailLink email
                    , maybe Null Company.companyLink company
                    ]
                  ]
                ]
              | Author {..} <- as
              ]
            ]     

