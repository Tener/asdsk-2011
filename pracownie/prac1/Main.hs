{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Main where

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
import System.Console.CmdArgs
import Data.Function
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word
import Data.Monoid
import Control.Concurrent.STM

data Config = Config { helloInterval :: Double
                     , address :: String
                     , port :: Int
                     , dieTime :: Double
                     }

data OtherSystem = OSyst { lastHeard :: Time -- ^ last time the system was heard from
                         , interestedIn :: Interests -- ^ lists of items this system is interested in
                         , timeoutThread :: ThreadID -- ^ thread that will kill this entry upon timeout
                         }

instance Default Config where
    def = fix (\ self -> Config { helloInterval = 30
                                , address = "224.0.0.213"
                                , port = 6969
                                , dieTime = 6 * (helloInterval self) })

type Interest = [Word8]
type Interests = Set Interest

type RevocationList = PriorityQueue Time IpAddr
type OtherSystems = Map IpAddr OtherSystem

-- | state of a running daemon.
data DaemonState = DState { configuration :: Config -- ^ current configuration
                          , conversants :: TVar OtherSystems -- ^ list of systems that have send their interests. will be mutated by 
                                                             --   other threads, hence 'TVar'.
                          , myInterests :: Interests -- ^ our own interests
                          }

serializeInterests :: Interests -> Builder
serializeInterests (Set.toList -> ints) = 
  fromInt32be (fromIntegral (length ints)) `mappend`
  (mconcat $ map (\ i -> fromWord8s i `mappend` fromWord8 0 ) ints)

updateFromSystem :: IpAddr -> Time -> Interests -> IO ()
updateFromSystem addr time ints = 

main = return ()


