{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module BasicServer where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (throwE)
import           Data.Int (Int64)
import           Data.Proxy (Proxy(..))
import           Database.Persist.Postgresql (ConnectionString)
import           Network.Wai.Handler.Warp (run)
import           Servant.API
import           Servant.Server
import           Database (fetchUserPG, createUserPG, localConnString, updateUserPG, deleteUserPG, insertManyUsersPG)
import           BasicSchema

-- endpoints 
type UsersAPI =
       "users" :> Capture "userid" Int64 :> Get '[JSON] User
  :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64
  :<|> "users" :> Capture "userid" Int64 :> ReqBody '[JSON] User :> Post '[JSON] SuccessResponse 
  :<|> "users" :> Capture "userid" Int64 :> Delete '[JSON] SuccessResponse 
  -- :<|> "users" :> ReqBody '[JSON] UserRequest :> Post '[JSON] SuccessResponse


usersAPI :: Proxy UsersAPI
usersAPI = Proxy :: Proxy UsersAPI

-- fetch handler
fetchUsersHandler :: ConnectionString -> Int64 -> Handler User
fetchUsersHandler connString uid = do
  maybeUser <- liftIO $ fetchUserPG connString uid
  case maybeUser of
    Just user -> return user
    Nothing -> Handler (throwE $ err401 { errBody = "Could not find user with that ID" })

-- create handler
createUserHandler :: ConnectionString -> User -> Handler Int64
createUserHandler connString user = liftIO $ createUserPG connString user

-- update handler
updateUserHandler :: ConnectionString -> Int64 -> User -> Handler SuccessResponse
updateUserHandler connString uid user = do
  maybeExistingUser <- liftIO $ fetchUserPG connString uid
  case maybeExistingUser of
    Just _ -> do
        _ <- liftIO $ updateUserPG connString uid user
        return SuccessResponse { message = "Update successful!" }
    Nothing -> return SuccessResponse { message = "Could not find user with that ID" } 

-- delete handler
deleteUserHandler :: ConnectionString -> Int64 -> Handler SuccessResponse
deleteUserHandler connString uid = do
  maybeExistingUser <- liftIO $ fetchUserPG connString uid
  case maybeExistingUser of
    Just _ -> do
        _ <- liftIO $ deleteUserPG connString uid
        return SuccessResponse { message = "Deleted successful!" }
    Nothing -> return SuccessResponse { message = "Could not find user with that ID" } 

-- insertMany users 
-- insertManyUsersHandler :: ConnectionString -> UserRequest -> Handler SuccessResponse
-- insertManyUsersHandler connString usrReq = do
--   _ <- liftIO $ print (show usrReq)
--   _ <- liftIO $ insertManyUsersPG connString usrReq
--   return SuccessResponse { message = "All Insert done successfully!" }


usersServer :: ConnectionString -> Server UsersAPI
usersServer connString =
  fetchUsersHandler connString :<|>
  createUserHandler connString :<|>
  updateUserHandler connString :<|>
  deleteUserHandler connString 
  -- :<|>
  -- insertManyUsersHandler connString

runServer :: IO ()
runServer = run 8000 (serve usersAPI (usersServer localConnString))

