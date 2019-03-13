{-# LANGUAGE DeriveGeneric #-}
module Main where

import Control.Comonad.Cofree
import qualified Data.ByteString as B
import GHC.Generics (Generic)

class Monad m => MonadNetwork m where
  send :: B.ByteString -> m ()
  recv :: m B.ByteString

type Moore a b = Cofree ((->) a) b

data State = Leader | Follower | Candidate deriving (Show, Generic)

data Action = Ping

-- State transitions
onLeader :: Action -> Moore Action State
onLeader Ping = Leader :< onLeader

onFollower :: Action -> Moore Action State
onFollower Ping = Follower :< onFollower

onCandidate :: Action -> Moore Action State
onCandidate Ping = Follower :< onFollower

main :: IO ()
main = putStrLn "Hello, Haskell!"
