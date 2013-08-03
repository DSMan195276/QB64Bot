module LineParse ( lineParse ) where

import IrcBot
import Types
import Cmds

import Data.List
import System.Time
import Data.IORef
import Data.List.Split
import Control.Monad.Reader


lineParse :: String -> Net ()
lineParse s = do
     if ping s 
       then pong s 
       else if topicChange s
         then liftIO $ topicSet s
         else do 
	   let test = (getIRCMessage s) --return () --eval (clean s) (getChan s) (getUser s)
	   liftIO $ putStrLn (show test)
	   eval test
  where
    clean         = drop 1 . dropWhile (/= ':') . drop 1
    getChan a     = let ch = (takeWhile (/= ':') . drop 9 . dropWhile (/= ' ')) a
                      in if (ch == nick)
                           then (takeWhile (/= '!') . (drop 1)) a
                           else ch

    getUser       = takeWhile (/= '!') . drop 1
    topicChange a = (getResponseNum a) == "332" 
    topicSet    a = writeIORef topic (splitOn " | " ((drop 1 . dropWhile (/= ':') . drop 1) a))
    ping x        = "PING :" `isPrefixOf` x
    pong x        = write "PONG" (':' : drop 6 x)
    
getResponseNum :: String -> String
getResponseNum = takeWhile (/= ' ') . drop 1 . dropWhile (/= ' ')


getIRCMessage :: String -> IRCMessage
getIRCMessage raw = getIRCMessage' raw (IRCMessage { fullText=raw, command="", responseNum="", channel="", user="", semiText="" })

getIRCMessage' :: String -> IRCMessage -> IRCMessage
getIRCMessage' []        m = m
getIRCMessage' (':':raw) m = if ((command m) == "")
                               then getIRCMessage' (drop (length (getUser raw)) raw) (m { user = (getUser raw) } )
                               else (m { semiText = raw })
                           where
                             getUser = takeWhile (/= '!')
getIRCMessage' raw m = m


eval :: IRCMessage -> Net ()
eval m = eval' m cmdList

eval' :: IRCMessage -> [IRCCommand] -> Net ()
eval' _ []     = return ()
eval' m@(IRCMessage { fullText=x }) ((IRCCommand { commandName = name, commandFunction = func}):cmds) = 
                            if ((name) `isPrefixOf` x) then (func) m else eval' m cmds

