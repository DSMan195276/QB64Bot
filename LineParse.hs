module LineParse ( lineParse ) where

import IrcBot
import Types
import Cmds

import Data.List
import System.Time
import Control.Concurrent.MVar
import Data.List.Split
import Control.Monad.Reader

lineParse :: String -> Net ()
lineParse s 
    | ("PING :" `isPrefixOf` s)      = write "PONG" (':' : drop 6 s)
    | (rnum == "332")                  = liftIO (takeMVar topic) >> liftIO (putMVar topic (splitOn " | " ((drop 1 . dropWhile (/= ':') . drop 1) s)))
    | otherwise                      = do
                                  let msg = (getIRCMessage s)
                                  liftIO $ putStrLn (show msg)
                                  eval msg
  where
    rnum = getResponseNum s

getResponseNum :: String -> String
getResponseNum s = (takeWhile (/= ' ') . drop 1 . dropWhile (/= ' ')) s


getIRCMessage :: String -> IRCMessage
getIRCMessage raw = IRCMessage { fullText = raw, command = (readCommand raw), rightText = (readRight raw), channel = (readChannel raw), user = (readUser raw) , semiText = reverse (takeWhile (/= ':') (reverse raw)) }
    where
      readUser    r = if ((take 1 r) == ":")
                        then (takeWhile (/= '!') . drop 1) r
                        else ""
      
      readChannel r = if ((cm r) == nick)
                        then readUser r
                        else cm r
        where cm r = (takeWhile (/= ' ') . drop 1 . dropWhile (/= ' ') . removeLeft) r
      readCommand r = (takeWhile (/= ' ') . removeLeft) r
      readRight   r = (drop 1 . dropWhile (/= ' ') . removeLeft) r

      removeLeft  r = if ((take 1 r) == ":")
                        then (drop 1 . dropWhile (/= ' ')) r
                        else r

eval :: IRCMessage -> Net ()
eval m = eval' m cmdList

eval' :: IRCMessage -> [IRCCommand] -> Net ()
eval' _                             [] = return ()
eval' m@(IRCMessage { semiText=x }) ((IRCCommand { name = nam, function = func}):cmds) = 
                            if ((nam) `isPrefixOf` (drop 1 x)) then (func) m else eval' m cmds

