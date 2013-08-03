module Main(main) where

import Data.List
import Network
import System.IO
import System.Time
import Control.Monad.Reader

import Control.Exception
import Text.Printf

import Types
import IrcBot
import Cmds
import LineParse


main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop st    = (runReaderT run st) 

listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` (liftIO (hGetLine h))
    liftIO $ putStrLn s
    lineParse s
  where
    forever a = a >> forever a

connect :: IO Bot
connect = notify $ do
    hSetBuffering stdout LineBuffering
    t <- getClockTime
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return (Bot h t)
  where
    notify a = bracket_
        (printf "Connecting to %s ... " server >> hFlush stdout)
        (putStrLn "done.")
        a

run :: Net ()
run = do
    fhand <- liftIO $ openFile "./qb64_pass" ReadMode
    pass <- liftIO $ hGetLine fhand
    liftIO $ hClose fhand
    write "NICK" nick
    write "USER" (nick++" 0 * :QB64 Bot")
    write "JOIN" chan
    privmsg "nickserv" ("identify " ++ pass)
    asks socket >>= listen
