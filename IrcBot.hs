module IrcBot ( server
              , port
              , chan
              , nick
              , topic
              , owners
              , privmsg
              , write
              )
              where

import Types

import Data.List
import Network
import System.IO
import System.Time
import System.Exit
import Control.Concurrent.MVar
import System.IO.Unsafe
import Data.List.Split
import Control.Monad.Reader

import Control.Exception
import Text.Printf

server = "irc.freenode.net"
port   = 6667
chan   = "#qb64"
nick   = "QB64Bot2"


{-# NOINLINE topic #-}
topic :: MVar [String]
topic = unsafePerformIO (newMVar [""])

{-# NOINLINE owners #-}
owners :: MVar [String]
owners = unsafePerformIO (newMVar ["dsman195276"])

privmsg c s = write "PRIVMSG" (c ++ " :" ++ s)

write :: String -> String -> Net ()
write s t =
    asks socket >>= \h ->
    liftIO (hPrintf h "%s %s\r\n" s t) >>
    liftIO (printf    "> %s %s\n" s t)


