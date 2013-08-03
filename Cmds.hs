module Cmds where

import Types
import IrcBot

import Data.List
import Network
import System.Time
import System.Exit
import Data.IORef
import Data.List.Split
import Control.Monad.Reader

cmdList = [ IRCCommand { commandName="updatetopic",     commandFunction=cmdUpdateTopic,     commandDescription="Updates to topic to display the current Topic " ++ nick ++ " has (Must be Owner)" }
          , IRCCommand { commandName="uptime",          commandFunction=cmdUpTime,          commandDescription="Displays " ++ nick ++ " current uptime" }
          , IRCCommand { commandName="addtopic",        commandFunction=cmdAddTopic,        commandDescription="Adds any text after '!addtopic' as an entry onto the topic (Must be Owner)" }
          , IRCCommand { commandName="deltopic",        commandFunction=cmdDelTopic,        commandDescription="Removes the left-most entry in the current Topic (Must be Owner)" }
          , IRCCommand { commandName="echo",            commandFunction=cmdEcho,            commandDescription="Makes " ++ nick ++ " say any text after '!echo'" }
          , IRCCommand { commandName="quit",            commandFunction=cmdQuit,            commandDescription="Make " ++ nick ++ " Exit the " ++ chan ++ " and quit (Must be Owner)" }
          , IRCCommand { commandName="displayowners",   commandFunction=cmdDisplayOwners,   commandDescription="Show a list of all the owners" }
          , IRCCommand { commandName="help",            commandFunction=cmdHelp,            commandDescription="Display usage and list of commands" }
          ]


cmdUpdateTopic :: IRCMessage -> Net ()
cmdUpdateTopic (IRCMessage { user = usr })= 
    liftIO (usrIsOwner usr) >>= \isUser -> if isUser then updateTopic topic else return ()

cmdUpTime :: IRCMessage -> Net ()
cmdUpTime (IRCMessage { channel = ch }) = uptime >>= privmsg (ch)

cmdAddTopic :: IRCMessage -> Net ()
cmdAddTopic (IRCMessage { fullText = x, user = usr }) = 
    liftIO (usrIsOwner usr) >>= \isUser -> 
    if isUser
      then liftIO $ addTopic (drop 10 x) topic
      else return ()

cmdDelTopic :: IRCMessage -> Net ()
cmdDelTopic (IRCMessage { user = usr }) = 
    liftIO (usrIsOwner usr) >>= \isUser ->
    if isUser
      then liftIO $ delTopic topic
      else return ()

cmdEcho :: IRCMessage -> Net ()
cmdEcho (IRCMessage { fullText = x, channel = ch }) = privmsg (ch) (drop 6 x)

cmdQuit :: IRCMessage -> Net ()
cmdQuit (IRCMessage { user = usr }) = 
    liftIO (usrIsOwner usr) >>= \isUser ->
    if isUser
      then write "QUIT" ":Exiting" >> (liftIO (exitWith ExitSuccess))
      else return ()

cmdHelp :: IRCMessage -> Net ()
cmdHelp (IRCMessage { fullText = x, user = usr }) = do
    privmsg usr ("To use me, type one of the below commands as a message. Include any text after the command as nessisary")
    displayOwners usr
    cmdHelp' usr cmdList

cmdHelp' :: String -> [IRCCommand] -> Net ()
cmdHelp' _ [] = return ()
cmdHelp' usr (cmd:cmds) = privmsg usr (formatCmd cmd) >> cmdHelp' usr cmds
    where
      formatCmd (IRCCommand { commandName = name, commandDescription = desc }) = (name) ++ " -- " ++ (desc)

cmdDisplayOwners :: IRCMessage -> Net ()
cmdDisplayOwners (IRCMessage { channel = ch }) = displayOwners ch

usrIsOwner :: String -> IO Bool
usrIsOwner usr = readIORef owners >>= \own -> return (usr `elem` own)

uptime :: Net String
uptime = do
    now  <- liftIO $ getClockTime
    zero <- asks starttime
    return . pretty $ diffClockTimes now zero

pretty :: TimeDiff -> String
pretty td = join . intersperse " " . filter (not . null) . map f $
    [ (years         ,"y") ,(months `mod` 11,"m")
    , (days  `mod` 18,"d") ,(hours  `mod` 24,"h")
    , (mins  `mod` 60,"m") ,(secs   `mod` 60,"s")
    ]
  where
    secs    = abs $ tdSec td  ; mins   = secs   `div` 60
    hours   = mins   `div` 60 ; days   = hours  `div` 24
    months  = days   `div` 28 ; years  = months `div` 12
    f (i,s) | i == 0    = []
            | otherwise = show i ++ s

formatTopic :: [String] -> String
formatTopic t = intercalate " | " t

addTopic :: String -> IORef [String] -> IO ()
addTopic x t = do
    top <- readIORef t
    writeIORef t (x : top)


delTopic :: IORef [String] -> IO ()
delTopic t = do
    top <- readIORef t
    writeIORef t (drop 1 top)

updateTopic :: IORef [String] -> Net ()
updateTopic t = do
    top <- liftIO $ readIORef t
    liftIO $ putStrLn ("NewTopic: " ++ head top)
    write "TOPIC" (chan ++ " :" ++ formatTopic top)

numberList :: [String] -> [String]
numberList []  = []
numberList lis = numberList' lis 1

numberList' :: [String] -> Int -> [String]
numberList' []     n = []
numberList' (x:xs) n = ((show n) ++ ". " ++ x) : numberList' xs (n+1)

displayOwners :: String -> Net ()
displayOwners ch = do
    o <- liftIO $ readIORef owners
    privmsg ch ("Owners: " ++ (intercalate " " (numberList o)))


