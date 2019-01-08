module Main where

import Pure (time,pattern Null)

import Router
import Scope

import Pages.Blog
import Pages.Doc
import Pages.Docs
import Pages.Examples
import Pages.Home
import Pages.Post
import Pages.Tutorial
import Pages.Tutorials

setup :: AppScope => IO ()
setup = return ()

pages pg =
  case pg of
    NoR       -> Null
    HomeR     -> homePage
    BlogR     -> blogPage
    PostR s   -> usingPost s postPage
    DocsR     -> docsPage
    DocR p v  -> usingDoc (p,v) docPage
    TutsR     -> tutorialsPage
    TutR s    -> usingTut s tutorialPage
    ExamplesR -> examplesPage

main :: IO ()
main = do
  now <- time
  Scope.run (State now) NoR Router.router setup id pages
