{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
module Main where

import           Control.Comonad.Cofree
import           Control.Monad.Reader   (MonadReader, ask)
import           Control.Monad.State    (MonadState, modify)
import           Data.Binary            (Binary)
import qualified Data.Binary            as B
import qualified Data.ByteString.Lazy   as B
import           Data.Either            (either)
import           GHC.Generics           (Generic)

hush :: Either e a -> Maybe a
hush = either (const Nothing) Just

class Monad m => MonadNetwork m where
  send :: B.ByteString -> m ()
  recv :: m B.ByteString

recvMsg :: MonadNetwork m => m (Maybe Msg)
recvMsg = fmap val . hush . B.decodeOrFail <$> recv
  where
    val (_,_,a) = a

class Applicative f => Race f where
  race :: f a -> f a -> f a

class Monad m => Concurrent m where
  wait :: Int -> m ()

type Moore a b = Cofree ((->) a) b

newtype Node id = Node { identifier :: id }

newtype NodeState a = NodeState { log :: [a] } deriving (Show, Functor, Foldable)

data RaftState = Leader | Follower | Candidate deriving (Show, Generic)

data Action = Ping | Vote

data Msg = PingMsg deriving (Generic)

instance Binary Msg

-- RaftState transitions
onLeader :: Action -> Moore Action RaftState
onLeader Ping = Leader :< onLeader

onFollower :: Action -> Moore Action RaftState
onFollower Ping = Follower :< onFollower

onCandidate :: Action -> Moore Action RaftState
onCandidate Ping = Follower :< onFollower

leader :: Monad m => m Action
leader = undefined

follower :: Monad m => m Action
follower = undefined

candidate :: Monad m => m Action
candidate = undefined

run :: (MonadState (NodeState a) m, MonadReader (Node id) m, Monad m) => Moore Action RaftState -> m ()
run = \case
  Leader :< next -> leader >>= run . next
  Follower :< next -> follower >>= run . next
  Candidate :< next -> candidate >>= run . next

main :: IO ()
main = putStrLn "Hello, Haskell!"
