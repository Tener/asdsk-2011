{-# LANGUAGE OverloadedStrings, ViewPatterns, ScopedTypeVariables #-}

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
-- network-multicast
import Network.Multicast
-- utility-ht
import Text.Read.HT (maybeRead)
-- base etc.
import Data.Function
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Word
import Data.Char
import Data.Monoid
import Control.Concurrent.STM
import Control.Concurrent
import System.Environment
import System.FilePath
import System.IO as IO
import Control.Monad
import Data.List(sort)

-- for tests only:
import Data.ByteString.Char8 ()

-- local imports
import DataTypes
import Parser

serializeInterests :: Interests -> Builder
serializeInterests (Set.toList -> ints) =
  fromInt32be (fromIntegral (length ints)) `mappend`
  (mconcat $ map (\ i -> fromWord8s i `mappend` fromWord8 0 ) ints)

interestFromString :: String -> Interest
interestFromString = map (fromIntegral . ord)

interestToString :: Interest -> String
interestToString = map (chr . fromIntegral)

operToFunction Add = Set.union
operToFunction Del = Set.difference

runDaemon :: IO ()
runDaemon = do
  config <- cmdArgs def
  conversants <- newTVarIO Map.empty
  myInt <- newTVarIO (Set.fromList $ map interestFromString $ words "kawa herbata sen")
  quitTVar <- newTVarIO False
  let whileGo f = do
              b <- readTVarIO quitTVar
              unless b f
  let _dstate = DState config conversants myInt

  let -- UDP server
      server_udp = do

              let send = do
                     (sock,addr) <- multicastSender (mcastAddress config) (fromIntegral (port config))
                     setLoopbackMode sock noLoopback
                     let loop = whileGo $ do
                           let msg = "Hello, world"
                           print ("sending",msg)
                           sendTo sock msg addr
                           Timeout.threadDelay (fromIntegral (helloInterval config) Timeout.# Second)
                           loop
                     loop
                  rec = do
                     sock <- multicastReceiver (mcastAddress config) (fromIntegral (port config))
                     let loop = whileGo $ do
                           foo <- recvFrom sock 1024
                           print ("received", foo)

                           loop
                     loop

              forkIO send
              forkIO rec

              return ()

      server_udp' = do
              sock <- socket AF_INET Datagram defaultProtocol -- (mcastAddress config)
              addr <- inet_addr (mcastAddress config)
              let socketAddress = SockAddrInet (fromIntegral (port config)) addr
              let -- UDP receiver thread
                  go_rec = do
                        (payload, (Sender -> sender)) <- recvFrom sock 65507
                        -- try parse, update record
                        print ("received",payload,"from",sender)
                        now <- getCurrentTime
                        let parsed = parseInterests payload
                        case parsed of
                          Left err -> print ("parse error",err)
                          Right ints | Set.null ints -> do
                                      print ("empty record received")
                                      oldThread <- fmap (Map.lookup sender) (readTVarIO conversants)
                                      case oldThread of
                                        Nothing -> return ()
                                        Just (timeoutThread -> tid) -> killThread tid
                                      atomically $ do
                                        convInfo <- readTVar conversants
                                        writeTVar conversants (Map.delete sender convInfo)
                                     | otherwise -> do
                                      print ("parse ok",ints,sender)
                                      -- check if the record has been present. if yes, kill the old thread. in either case run a new timeout-thread
                                      oldThread <- fmap (Map.lookup sender) (readTVarIO conversants)
                                      case oldThread of
                                        Nothing -> return ()
                                        Just (timeoutThread -> tid) -> killThread tid
                                      let timeout = (fromIntegral (dieTime config * helloInterval config)) Timeout.# Second
                                      tid <- forkIO $ do
                                          Timeout.threadDelay timeout
                                          atomically $ do
                                                 convInfo <- readTVar conversants
                                                 writeTVar conversants (Map.delete sender convInfo)
                                      atomically $ do
                                          convInfo <- readTVar conversants
                                          writeTVar conversants (Map.insert sender (Other { lastHeard = now
                                                                                          , interestedIn = ints
                                                                                          , timeoutThread = tid }) convInfo)


                        go_rec
                  -- UDP sender thread
                  go_send = do
                        sendInterestsNow
                        Timeout.threadDelay (fromIntegral (helloInterval config) Timeout.# Second)
                        go_send
                  -- send current interests NOW
                  sendInterestsNow = do
                     int <- readTVarIO myInt
                     let msg = (toByteString $ serializeInterests int)
                     print ("sending now", msg)
                     sendAllTo sock msg socketAddress

              bindSocket sock socketAddress
              forkIO go_rec
              forkIO go_send

      -- AF_UNIX server for CLI
      server_cli = do
            -- sock <- socket AF_UNIX Stream defaultProtocol
            -- let socketAddress = SockAddrUnix (localSocketPath config)
            -- bindSocket sock socketAddress
            sock <- listenOn (UnixSocket (localSocketPath config))
            let loop = whileGo $ do
                  (conn, _, _) <- Network.accept sock
                  cont <- IO.hGetLine conn
                  print ("received",cont)
                  case maybeRead cont of
                    Nothing -> print ("malformed command",cont)
                    Just Quit -> atomically $ writeTVar quitTVar True
                    Just (Oper op add) -> do
                                   mine <- atomically $ do
                                        mi <- readTVar myInt
                                        let mi' = (operToFunction op mi (Set.singleton add))
                                        writeTVar myInt mi'
                                        return mi'
                                   IO.hPrint conn mine
                    Just InfoMy -> do
                      mi <- readTVarIO myInt
                      IO.hPrint conn ("mine",mi)
                    Just InfoFull -> do
                      let fun oh = OtherInfo (lastHeard oh) (interestedIn oh)
                      convInfo <- readTVarIO conversants
                      IO.hPrint conn (Map.mapKeys show $ (Map.map fun convInfo))

                  IO.hFlush conn
                  print =<< readTVarIO myInt
                  hClose conn
                  loop
            loop
            sClose sock

  forkIO server_udp
  server_cli

  Timeout.threadDelay (1 Timeout.# Day)

updateFromSystem :: Host -> Interests -> IO ()
updateFromSystem addr ints = do
  now <- getCurrentTime
--  atomically $ do
--    conversants

  return ()

---------- CLI part

-- | helper for pretty printing
showInterestsAddr :: String -> Interests -> IO ()
showInterestsAddr whom (Set.toList -> ints) = do
  putStrLn whom
  mapM_ (\is -> do
           putStr "   "
           putStrLn (interestToString is)) ints

-- | get information on all the systems (update time, interests)
getFullInfo handle = do
  IO.hPrint handle InfoFull
  IO.hFlush handle
  cont <- hGetLine handle
  case maybeRead cont of
    Nothing -> error ("błąd parsowania (getFullInfo): " ++ show cont)
    Just (others :: OtherHostsInfo) -> mapM_ (\(host,info) -> do
                                            let header = show host ++ show (interestedIn' info)
                                            showInterestsAddr header (interestedIn' info)
                                         )
                                         (Map.assocs others) -- showInterestsAddr "Current set of interests:" ints

-- | get system on selected interest (hosts that are interested in it)
getSelectedInfo handle topic = do
  IO.hPrint handle InfoFull
  IO.hFlush handle
  cont <- hGetLine handle
  case maybeRead cont of
    Nothing -> error "błąd parsowania (getSelectedInfo)"
    Just (others :: OtherHostsInfo) -> do
                                 let filtered = Map.filter (Set.member topic . interestedIn') others
                                     hosts = sort (Map.keys filtered)
                                 mapM_ print hosts

-- | update our interests, get status report for our system
sendUpdateGetMine handle msg = do
  IO.hPrint handle msg
  IO.hFlush handle
  cont <- IO.hGetLine handle
  case maybeRead cont of
    Nothing -> error "błąd parsowania (sendGetCurrent)"
    Just ints -> showInterestsAddr "Current set of interests:" ints

runCLI :: IO ()
runCLI = do
  config <- cmdArgs def
  handle <- connectTo "" (UnixSocket (localSocket config))
  let ord' = map (fromIntegral . ord)
  case config of
    (quit -> True) -> IO.hPrint handle Quit
    (interestFromString . info -> str) | not (null str) -> getSelectedInfo handle str
    (interestFromString . add -> str) | not (null str) -> sendUpdateGetMine handle (Oper Add str)
    (interestFromString . del -> str) | not (null str) -> sendUpdateGetMine handle (Oper Del str)
    _ -> getFullInfo handle
  IO.hClose handle

---------- Main program, dispatcher

main :: IO ()
main = withSocketsDo $ do
   prog <- getProgName
   case prog of
     "pwzd" -> runDaemon
     "pwz" -> runCLI
     _ -> return ()
