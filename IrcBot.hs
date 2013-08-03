module IrcBot where

import Types

import Data.List
import Network
import System.IO
import System.Time
import System.Exit
import Data.IORef
import System.IO.Unsafe
import Data.List.Split
import Control.Monad.Reader

import Control.Exception
import Text.Printf

server = "irc.freenode.net"
port   = 6667
chan   = "#qb64"
nick   = "QB64Bot2"


topic :: IORef [String]
topic = unsafePerformIO ( newIORef [""] )

owners :: IORef [String]
owners = unsafePerformIO ( newIORef ["dsman195276"] )


privmsg :: String -> String -> Net ()
privmsg c s = write "PRIVMSG" (c ++ " :" ++ s)

write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    liftIO $ hPrintf h "%s %s\r\n" s t
    liftIO $ printf    "> %s %s\n" s t

