{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE DeriveFunctor #-}
module Main where

import           Control.Comonad.Cofree
import           Control.Monad.Reader   (MonadReader)
import           Control.Monad.State    (MonadState, modify, gets)
import           Data.Binary            (Binary)
import qualified Data.Binary            as B
import qualified Data.ByteString.Lazy   as B
import           Data.Either            (either)
import           GHC.Generics           (Generic)
import           System.Random          (Random)

hush :: Either e a -> Maybe a
hush = either (const Nothing) Just

-- I haven't introduced networking yet so this is dummy value for now
data NetworkTarget

class Monad m => MonadRandom m where
  randomR :: (Random a) => (a,a) -> m a

class Monad m => MonadNetwork m where
  send :: NetworkTarget -> B.ByteString -> m ()
  recv :: m (NetworkTarget, B.ByteString)

recvMsg :: MonadNetwork m => m (Maybe (NetworkTarget, Msg))
recvMsg = traverse (fmap val . hush . B.decodeOrFail) <$> recv
  where
    val (_,_,a) = a

sendMsg :: MonadNetwork m => Msg -> NetworkTarget -> m ()
sendMsg _msg _dunnowhattodowiththisyet = undefined

class Applicative f => Race f where
  race :: f a -> f a -> f a

class Monad m => Concurrent m where
  wait :: Int -> m ()

type Moore a b = Cofree ((->) a) b

newtype Node id = Node { identifier :: id }

data NodeState a = NodeState { log :: [a]
                             , peers :: forall id. [(Node id, NetworkTarget)]
                             }
                 deriving (Functor)

data RaftState = Leader | Follower | Candidate deriving (Show, Generic)

data Action = Ping | Vote

data Msg = PingMsg | AppendLog Msg deriving (Generic)

instance Binary Msg

-- RaftState transitions
onLeader :: Action -> Moore Action RaftState
onLeader Ping = Leader :< onLeader

onFollower :: Action -> Moore Action RaftState
onFollower Ping = Follower :< onFollower

onCandidate :: Action -> Moore Action RaftState
onCandidate Ping = Follower :< onFollower

waitMajority :: Monad m => [a] -> m ()
waitMajority _ = undefined

leader :: (MonadState (NodeState Msg) m, MonadRandom m, Race m, Concurrent m, MonadNetwork m) => m Action
leader = do
  p <- fmap snd <$> gets peers
  traverse (sendMsg PingMsg) p
  waitTime <- randomR (100000, 300000)
  msg <- race (Left () <$ wait waitTime) (Right <$> recvMsg)
  case msg of
       Left _ -> pure Ping
       Right (Just (_sender, msg)) -> do
         traverse (sendMsg (AppendLog msg)) p >> waitMajority p
         pure Ping

follower :: Monad m => m Action
follower = undefined

candidate :: Monad m => m Action
candidate = undefined

run ::
  ( MonadRandom m
  , Race m
  , Concurrent m
  , MonadNetwork m
  , MonadState (NodeState Msg) m
  , MonadReader (Node id) m
  , Monad m) => Moore Action RaftState -> m ()
run = \case
  Leader :< next -> leader >>= run . next
  Follower :< next -> follower >>= run . next
  Candidate :< next -> candidate >>= run . next

main :: IO ()
main = putStrLn "Hello, Haskell!"
