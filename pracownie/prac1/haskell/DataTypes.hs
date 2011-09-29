{-# LANGUAGE DeriveDataTypeable, ViewPatterns #-}
module DataTypes where

import Data.Time(UTCTime)
import Network(HostName)
import Network.Socket(SockAddr)
import Data.Map (Map)
import Data.Set (Set)
import Data.Data
import Control.Concurrent (ThreadId)
import System.Console.CmdArgs (Default(..))
import Control.Concurrent.STM (TVar)
import Data.Word (Word8)


type Host = SockAddr
type Time = UTCTime

data Operation = Add | Del deriving (Show,Read)
data CLICommand = InfoFull | InfoMy | Quit | Oper Operation Interest deriving (Show,Read)

-- cli config
data CConfig = CConfig { localSocket :: FilePath
                       , quit :: Bool
                       , add :: String
                       , del :: String
                       , info :: String
                       }
             deriving (Data,Typeable,Read,Show)

-- server config
data SConfig = SConfig { helloInterval :: Int -- ^ the length of hello interval - the time (in seconds) between sending our interests to others
                       , mcastAddress :: String -- ^ multicast address
                       , port :: Int -- ^ port to use
                       , dieTime :: Int -- ^ number of intervals before the host is killed by inactivity
                       , quiet :: Bool -- ^ should the deamon be quiet? true by default
                       , localSocketPath :: FilePath -- ^ path to local control socket
                       }
               deriving (Data,Typeable,Read,Show)

data OtherHost = Other { lastHeard :: Time -- ^ last time the host was heard from
                       , interestedIn :: Interests -- ^ lists of items this system is interested in
                       , timeoutThread :: ThreadId -- ^ thread that will kill this entry upon timeout
                       }
                 deriving (Show)

-- | almost the same as OtherHost, but without problematic timeout thread
data OtherHostInfo = OtherInfo { lastHeard' :: Time -- ^ last time the host was heard from
                               , interestedIn' :: Interests -- ^ lists of items this system is interested in
                               }
                     deriving (Data,Typeable,Read,Show)

instance Default SConfig where
    def = SConfig { helloInterval = 3
                 , mcastAddress = "224.0.0.213"
                 , port = 6969
                 , dieTime = 6
                 , quiet = True
                 , localSocketPath = "/tmp/pwz_tener.sock"
                 }

instance Default CConfig where
    def = CConfig { localSocket = localSocketPath def
                  , quit = False
                  , add = ""
                  , del = ""
                  , info = ""
                  }

type Interest = [Word8]
type Interests = Set Interest
type OtherHosts = Map Sender OtherHost
type OtherHostsInfo = Map String OtherHostInfo

-- | state of a running daemon.
data DaemonState = DState { configuration :: SConfig -- ^ current configuration
                          , conversants :: TVar OtherHosts -- ^ list of systems that have send their interests. will be mutated by
                                                           --   other threads, hence 'TVar'.
                          , myInterests :: TVar Interests -- ^ our own interests
                          }

newtype Sender = Sender { fromSender :: Host } deriving (Show,Eq)

instance Ord Sender where
    compare (show . fromSender -> a) (show . fromSender -> b) = compare a b
