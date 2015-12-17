{-# Language OverloadedStrings #-}
module Models where

import Database.HDBC
import Database.HDBC.Sqlite3

type Post = (Integer, String, String)

convFromSql :: [SqlValue] -> Post
convFromSql [sqlId, sqlName, sqlBody] =
    (id, name, body) 
    where id = (fromSql sqlId) :: Integer
          name = case fromSql sqlName of
                    Just x -> x
                    Nothing -> "" :: String
          body = case fromSql sqlBody of
                    Just x -> x
                    Nothing -> "" :: String
convRow x = fail $ "Unexpected result: " ++ show x

createPostsTable :: IO ()
createPostsTable = do
    conn <- connectSqlite3 "tinyblog.db"
    run conn (unwords ["CREATE TABLE IF NOT EXISTS posts (", 
                       "id INTEGER PRIMARY KEY AUTOINCREMENT,", 
                       "name TEXT,",
                       "body TEXT",
                       ")"]) []

    disconnect conn

getPosts :: IO [Post]
getPosts = do
    conn <- connectSqlite3 "tinyblog.db"
    results <- quickQuery' conn "SELECT * FROM posts ORDER BY id DESC LIMIT 10" []
    let rows = map convFromSql results

    disconnect conn

    return rows

getPost :: Int -> IO Post
getPost postId = do
    conn <- connectSqlite3 "tinyblog.db"
    results <- quickQuery' conn "SELECT * FROM posts WHERE id == ?" [toSql postId]
    let row = head $ map convFromSql results

    disconnect conn

    return row

createPost :: String -> String -> IO ()
createPost name body = do
    conn <- connectSqlite3 "tinyblog.db"
    run conn "INSERT INTO posts VALUES (?, ?, ?)"
             [SqlNull, toSql name, toSql body]
    commit conn

    disconnect conn

updatePost :: Int -> String -> String -> IO ()
updatePost id name body = do
    conn <- connectSqlite3 "tinyblog.db"
    run conn "UPDATE posts SET name = ?, body = ? WHERE id = ?"
             [toSql name, toSql body, toSql id]
    commit conn

    disconnect conn

deletePost :: Int -> IO ()
deletePost id = do
    conn <- connectSqlite3 "tinyblog.db"
    run conn "DELETE FROM posts WHERE id = ?"
             [toSql (id :: Int)]
    commit conn

    disconnect conn
