{-# LANGUAGE OverloadedStrings #-}
module Views where

import Lucid
import Data.Text

import Control.Monad

import Models

data Page = Index | PostPage | Other
    deriving (Eq)

parsePost :: Post -> Html ()
parsePost (id, name, body) = do
    h3_ $ do
        a_ [href_ (pack $ "/" ++ show id)] $ toHtml name
    p_ $ toHtml body

parsePosts :: [Post] -> Html ()
parsePosts ps = do
    forM_ ps (\ (id, name, body) -> do
                h3_ $ do
                    a_ [href_ (pack $ "/" ++ show id)] $ toHtml name
                p_ $ toHtml body)

inclusions :: Html ()
inclusions = do 
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/css/bootstrap.min.css"]
    link_ [rel_ "stylesheet", type_ "text/css", href_ "/css/bootstrap-theme.min.css"]
    script_ [type_ "text/javascript", src_ "/js/jquery-1.11.3.min.js"] ("" :: String)
    script_ [type_ "text/javascript", src_ "/js/bootstrap.min.js"] ("" :: String)

navbar :: Page -> Html ()
navbar page = 
    div_ [class_ "navbar navbar-default navbar-static-top"] $ do
        div_ [class_ "container"] $ do
            div_ [class_ "navbar-header"] $ do
                a_ [class_ "navbar-brand", href_ "/"] "Tiny Blog"
            div_ [id_ "navbar", class_ "navbar-collapse collapse"] $ do
                ul_ [class_ "nav navbar-nav"] $ do
                    if page == Index then do
                            li_ [class_ "active"] $ do
                                a_ [href_ "/"] "Home"
                        else do
                            li_ $ do
                                a_ [href_ "/"] "Home"
                ul_ [class_ "nav navbar-nav navbar-right"] $ do
                    li_ $ do
                        a_ [href_ "/login"] "Login"

pageBody :: Html () -> Html ()
pageBody body = 
            div_ [class_ "container"] $ do
                div_ [class_ "row"] $ do
                    div_ [class_ "col-xs-9"] $ do
                        div_ [class_ "panel panel-default"] $ do
                            div_ [class_ "panel-body"] $ do
                                body
                    div_ [class_ "col-xs-3"] $ do
                        div_ [class_ "panel panel-default"] $ do
                            div_ [class_ "panel-body"] $ do
                                p_ "Stuff"

index :: [Post] -> Html ()
index ps =
    html_ $ do
        let body = parsePosts ps
        head_ $ do
            inclusions
        body_ $ do
            navbar Index
            pageBody body

page :: Post -> Html ()
page p =
    html_ $ do
        let body = parsePost p
        head_ $ do
            inclusions
        body_ $ do
            navbar PostPage
            pageBody body

login :: Html ()
login = 
    html_ $ do
        head_ $ do
            inclusions
        body_ $ do
            navbar Other
            div_ [class_ "container"] $ do
                div_ [class_ "col-xs-4 col-xs-offset-4"] $ do

                    form_ [method_ "POST", action_ "/login"] $ do
                        div_ [class_ "form-group"] $ do
                            div_ [class_ "input-group"] $ do
                                input_ [class_ "form-control", name_ "username", type_ "text", placeholder_ "Username"]
                        div_ [class_ "form-group"] $ do
                            div_ [class_ "input-group"] $ do
                                input_ [class_ "form-control", name_ "password", type_ "password", placeholder_ "Password"]
                        button_ [class_ "btn btn-default pull-right", type_ "submit"] "Submit"

admin :: [Post] -> Html ()
admin ps = 
    html_ $ do 
        head_ $ do
            inclusions
        body_ $ do
            navbar Other
            pageBody $ (do
                        table_ [class_ "table"] $ do
                            caption_ "Existing posts"
                            thead_ $ do
                                tr_ $ do
                                    th_ "Name"
                                    th_ "Body"
                            tbody_ $ do
                                forM_ ps (\ (id, name, body) -> do
                                    tr_ $ do 
                                        td_ $ do
                                            a_ [href_ (pack $ "/edit/" ++ show id)] $ toHtml name
                                        td_ $ toHtml $ (Prelude.take 40 body) ++ " ...")
                        a_ [href_ "/new"] $ do
                            button_ [class_ "btn btn-success pull-right"] "New Post")
newPost :: Html ()
newPost =
    html_ $ do
        head_ $ do 
            inclusions
        body_ $ do
            navbar Other
            div_ [class_ "container"] $ do
                div_ [class_ "row"] $ do
                    div_ [class_ "col-xs-10 col-xs-offset-1"] $ do
                        div_ [class_ "panel panel-default"] $ do
                            div_ [class_ "panel-heading"] "Create a new post"
                            div_ [class_ "panel-body"] $ do
                                form_ [method_ "POST", action_ "/new"] $ do
                                    div_ [class_ "form-group"] $ do
                                        div_ [class_ "input-group"] $ do
                                            input_ [class_ "form-control", name_ "name", type_ "text", placeholder_ "Post title"]
                                    div_ [class_ "form-group"] $ do
                                        textarea_ [class_ "form-control", rows_ "10", name_ "body"] "Post body"
                                    button_ [class_ "btn btn-success pull-right", type_ "submit"] "Submit"

editPost :: Post -> Html ()
editPost (id, name, body) =
    html_ $ do
        head_ $ do
            inclusions
        body_ $ do
            navbar Other
            div_ [class_ "container"] $ do
                div_ [class_ "row"] $ do
                    div_ [class_ "col-xs-10 col-xs-offset-1"] $ do
                        div_ [class_ "panel panel-default"] $ do
                            div_ [class_ "panel-heading"] "Create a new post"
                            div_ [class_ "panel-body"] $ do
                                form_ [method_ "POST", action_ (pack $ "/edit/" ++ show id)] $ do
                                    div_ [class_ "form-group"] $ do
                                        div_ [class_ "input-group"] $ do
                                            input_ [class_ "form-control", name_ "name", type_ "text", value_ (pack $ name)]
                                    div_ [class_ "form-group"] $ do
                                        textarea_ [class_ "form-control", rows_ "10", name_ "body"] (toHtml $ body)
                                    button_ [class_ "btn btn-success pull-right", type_ "submit"] "Submit"
                                    form_ [method_ "DELETE", action_ (pack $ "/edit/" ++ show id)] $ do
                                        button_ [class_ "btn btn-danger", type_ "submit"] "Delete"
