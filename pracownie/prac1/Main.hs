{-# LANGUAGE OverloadedStrings, ViewPatterns, DeriveDataTypeable #-}

module Main where

-- data-timeout
import Control.Concurrent.Timeout as Timeout
import Data.Timeout as Timeout
-- blaze-builder
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char.Utf8
-- cmdargs
import System.Console.CmdArgs
-- time
import Data.Time
-- network
import Network hiding (sendTo, recvFrom)
import qualified Network
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
-- base etc.
import Data.Function
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word
import Data.Monoid
import Control.Concurrent.STM
import Control.Concurrent
import System.Environment
import System.FilePath
import System.IO as IO

type Host = HostName
type Time = UTCTime

data Config = Config { helloInterval :: Int -- ^ the length of hello interval - the time (in seconds) between sending our interests to others
                     , mcastAddress :: Host -- ^ multicast address
                     , port :: Int -- ^ port to use
                     , dieTime :: Int -- ^ number of intervals before the host is killed by inactivity
                     , quiet :: Bool -- ^ should the deamon be quiet? true by default
                     , localSocketPath :: FilePath -- ^ path to local control socket
                     }
            deriving (Data, Typeable)

data OtherHost = Other { lastHeard :: Time -- ^ last time the host was heard from
                       , interestedIn :: Interests -- ^ lists of items this system is interested in
                       , timeoutThread :: ThreadId -- ^ thread that will kill this entry upon timeout
                       }

instance Default Config where
    def = Config { helloInterval = 30
                 , mcastAddress = "224.0.0.213"
                 , port = 6969
                 , dieTime = 6
                 , quiet = True
                 , localSocketPath = "/tmp/pwz_tener.sock"
                 }

type Interest = [Word8]
type Interests = Set Interest
type OtherHosts = Map Host OtherHost

-- | state of a running daemon.
data DaemonState = DState { configuration :: Config -- ^ current configuration
                          , conversants :: TVar OtherHosts -- ^ list of systems that have send their interests. will be mutated by 
                                                             --   other threads, hence 'TVar'.
                          , myInterests :: TVar Interests -- ^ our own interests
                          }

serializeInterests :: Interests -> Builder
serializeInterests (Set.toList -> ints) = 
  fromInt32be (fromIntegral (length ints)) `mappend`
  (mconcat $ map (\ i -> fromWord8s i `mappend` fromWord8 0 ) ints)

runDaemon :: IO ()
runDaemon = do
  config <- cmdArgs def
  conv <- newTVarIO Map.empty
  myInt <- newTVarIO Set.empty 
  let _dstate = DState config conv myInt

  let -- UDP server
      server_udp = do
              sock <- socket AF_INET Datagram defaultProtocol -- (mcastAddress config)
              addr <- inet_addr (mcastAddress config)
              let socketAddress = SockAddrInet (fromIntegral (port config)) addr
              let -- UDP receiver thread
                  go_rec = do
                        (payload, sender) <- recvFrom sock 65507
                        -- try parse, update record
                        print ("received",payload,"from",sender)
                        go_rec
                  -- UDP sender thread
                  go_send = do
                        Timeout.threadDelay (fromIntegral (helloInterval config) Timeout.# Second)
                        sendInterestsNow
                        go_send
                  -- send current interests NOW
                  sendInterestsNow = do
                     int <- readTVarIO myInt
                     sendAllTo sock (toByteString $ serializeInterests int) socketAddress

              bindSocket sock socketAddress
              forkIO go_rec
              forkIO go_send

      -- AF_UNIX server for CLI
      server_cli = do
            -- sock <- socket AF_UNIX Stream defaultProtocol
            -- let socketAddress = SockAddrUnix (localSocketPath config)
            -- bindSocket sock socketAddress
            sock <- listenOn (UnixSocket (localSocketPath config))
            let loop = do
                  (conn, _, _) <- Network.accept sock
                  cont <- IO.hGetContents conn
                  print ("received",cont)
                  hClose conn
                  loop
            forkIO loop
            return ()

  server_udp
  server_cli

  Timeout.threadDelay (1 Timeout.# Day)

updateFromSystem :: Host -> Interests -> IO ()
updateFromSystem addr ints = do
  now <- getCurrentTime
--  atomically $ do
--    conversants

  return ()
  
runCLI = do
  Network.sendTo ""  (UnixSocket (localSocketPath def)) "foo"
 

main = withSocketsDo $ do
   prog <- getProgName
   case prog of
     "pwzd" -> runDaemon
     "pwz" -> runCLI



