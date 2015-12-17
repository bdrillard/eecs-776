{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
module Main where

import Web.Scotty
import Network.Wai.Middleware.Static
import Lucid
import Data.Text
import Control.Monad
import Control.Monad.IO.Class (liftIO)

import qualified Models as Models
import qualified Views as Views

renderView = html . renderText

main = do 
    scotty 3000 $ do
        middleware $ staticPolicy (addBase "static")
        get "/" $ do
            ps <- liftIO $ Models.getPosts
            renderView $ Views.index ps

        get "/:id" $ do
            id <- param "id"
            p <- liftIO $ Models.getPost id
            renderView $ Views.page p

        get "/login" $ do
            renderView $ Views.login

        post "/login" $ do
            (username :: String) <- param "username"
            (password :: String) <- param "password"
            if username == "aeskilson@ku.edu" && password == "password" -- Super secure
                then do
                    redirect "/admin"
                else do
                    redirect "/login"

        get "/admin" $ do
            ps <- liftIO $ Models.getPosts
            renderView $ Views.admin ps

        get "/new" $ do
            renderView $ Views.newPost

        post "/new" $ do
            (name :: String) <- param "name"
            (body :: String) <- param "body"
            liftIO $ Models.createPost name body
            
            redirect "/admin"

        get "/edit/:id" $ do
            id <- param "id"
            p <- liftIO $ Models.getPost id
            renderView $ Views.editPost p

        post "/edit/:id" $ do
            id <- param "id"
            (name :: String) <- param "name"
            (body :: String) <- param "body"
            liftIO $ Models.updatePost id name body

            redirect "/admin"

        delete "/edit/:id" $ do
            id <- param "id"
            liftIO $ Models.deletePost id

            redirect "/admin"

