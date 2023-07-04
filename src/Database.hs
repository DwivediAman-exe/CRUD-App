{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Database where

import           Control.Monad.Logger
import           Control.Monad.Reader (runReaderT)
import           Data.Int (Int64)
import           Database.Persist
import           Database.Persist.Postgresql 
import           BasicSchema
import           Data.Text (pack)


type PGInfo = ConnectionString

-- pgsql connection string
localConnString :: PGInfo
localConnString = "host=127.0.0.1 port=5432 user=postgres dbname=postgres password=root"

-- SqlPersistT moand helps in accessing Postgresql database
-- The purpose of this function is to execute a SQL action on a PostgreSQL database using the provided connection string.
runAction :: PGInfo -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action = 
  runStdoutLoggingT $ filterLogger logFilter $ withPostgresqlConn connectionString $ \backend ->
    runReaderT action backend

-- migrate function to generate users table and migrate the DB 
migrateDB :: PGInfo -> IO ()
migrateDB connString = runAction connString (runMigration migrateAll)

-- logger
logFilter :: a -> LogLevel -> Bool
logFilter _ LevelError     = True
logFilter _ LevelWarn      = True
logFilter _ LevelInfo      = True
logFilter _ LevelDebug     = False
logFilter _ (LevelOther _) = False

-- fetch fuunction that select users with age less then 25 and occupation as teacher
fetchAllUsersPG :: PGInfo -> IO [Entity User]
fetchAllUsersPG connString = runAction connString (selectList [] [])

-- fetching a user from DB
fetchUserPG :: PGInfo -> Int64 -> IO (Maybe User)
fetchUserPG connString uid = runAction connString (get (toSqlKey uid))

-- Creating a user
createUserPG :: PGInfo -> User -> IO Int64
createUserPG connString user = fromSqlKey <$> runAction connString (insert user)
    
updateUserPG :: PGInfo -> Int64 -> User -> IO SuccessResponse
updateUserPG connString uid user = do
    let userKey = toSqlKey uid
    _ <- runAction connString (replace userKey user)
    return SuccessResponse { message = "Update successful!" }

-- Deleting a user
deleteUserPG :: PGInfo -> Int64 -> IO ()
deleteUserPG connString uid = runAction connString (delete userKey)
  where
    userKey :: Key User
    userKey = toSqlKey uid

-- insert Many users
insertManyUsersPG :: PGInfo -> [User] -> IO SuccessResponse
insertManyUsersPG connString userslist = do 
  -- logValue users
  _ <- runAction connString (insertMany_ userslist)
  return SuccessResponse { message = "All records inserted successfully!" }

-- query to fetch user qccording to their occupation
fetchUserOccupationPG :: PGInfo -> String -> IO [Entity User]
fetchUserOccupationPG connString occupation = runAction connString (selectList [UserOccupation ==. pack occupation] [])