
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
--import Prelude hiding (catch)

server = "irc.freenode.net"
port   = 6667
chan   = "#qb64"
nick   = "QB64Bot"
owners = ["dsman195276"]

type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle, starttime :: ClockTime }

topic :: IORef [String]
topic = unsafePerformIO ( newIORef [""] )

cmdList = [ ("updatetopic",     cmdUpdateTopic,"Updates to topic to display the current Topic " ++ nick ++ " has (Must be Owner)")
          , ("uptime",          cmdUpTime     ,"Displays " ++ nick ++ " current uptime")
          , ("addtopic",        cmdAddTopic   ,"Adds any text after '!addtopic' as an entry onto the topic (Must be Owner)")
	  , ("deltopic",        cmdDelTopic   ,"Removes the left-most entry in the current Topic (Must be Owner)")
	  , ("echo",            cmdEcho       ,"Makes " ++ nick ++ " say any text after '!echo'")
	  , ("quit",            cmdQuit       ,"Make " ++ nick ++ " Exit the " ++ chan ++ " and quit (Must be Owner)")
	  , ("help",            cmdHelp       ,"Display usage and list of commands")
	  ]

getCmdName :: (String, (String -> String -> String -> Net ()), String) -> String
getCmdName (nam, _, _) = "!" ++ nam

getCmdFunction :: (String, (String -> String -> String -> Net ()), String) -> (String -> String -> String -> Net ())
getCmdFunction (_, fun, _) = fun

getCmdDescription :: (String, (String -> String -> String -> Net ()), String) -> String
getCmdDescription (_, _, desc) = desc



cmdUpdateTopic :: String -> String -> String -> Net ()
cmdUpdateTopic _ _ usr = if (usr `elem` owners) then updateTopic topic else return ()

cmdUpTime :: String -> String -> String -> Net ()
cmdUpTime _ ch _ = uptime >>= privmsg (ch)

cmdAddTopic :: String -> String -> String -> Net ()
cmdAddTopic x _ usr = if (usr `elem` owners)
                        then io (addTopic (drop 10 x) topic)
			else return ()

cmdDelTopic :: String -> String -> String -> Net ()
cmdDelTopic _ _ usr = if (usr `elem` owners)
                        then io (delTopic topic)
			else return ()

cmdEcho :: String -> String -> String -> Net ()
cmdEcho x ch _ = privmsg (ch) (drop 6 x)

cmdQuit :: String -> String -> String -> Net ()
cmdQuit _ _ usr = if (usr `elem` owners)
                    then write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
		    else return ()

cmdHelp :: String -> String -> String -> Net ()
cmdHelp x ch usr = do
    privmsg usr ("To use me, type one of the below commands as a message. Include any text after the command as nessisary")
    privmsg usr ("Current Owners: " ++ intercalate " " owners)
    cmdHelp' usr cmdList

cmdHelp' :: String -> [(String, (String -> String -> String -> Net ()), String)] -> Net ()
cmdHelp' _ [] = return ()
cmdHelp' usr (cmd:cmds) = privmsg usr (formatCmd cmd) >> cmdHelp' usr cmds
    where
      formatCmd c = (getCmdName c) ++ " -- " ++ (getCmdDescription c)

main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop st    = (runReaderT run st) 

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
    pass <- io (hGetLine fhand)
    io (hClose fhand)
    write "NICK" nick
    write "USER" (nick++" 0 * :QB64 Bot")
    write "JOIN" chan
    privmsg "nickserv" ("identify " ++ pass)
    asks socket >>= listen

listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    io (putStrLn s)
    if ping s 
      then pong s 
      else if topicChange s
        then io (topicSet s)
	else eval (clean s) (getChan s) (getUser s)
  where
    forever a     = a >> forever a
    clean         = drop 1 . dropWhile (/= ':') . drop 1
    getChan a     = if isInfixOf nick a
                      then (takeWhile (/= '!') . (drop 1)) a
                      else (takeWhile (/= ':') . drop 9 . dropWhile (/= ' ')) a
    getUser       = takeWhile (/= '!') . drop 1
    topicChange a = (getResponseNum a) == "332" 
    topicSet    a = writeIORef topic (splitOn " | " ((drop 1 . dropWhile (/= ':') . drop 1) a))

    ping x        = "PING :" `isPrefixOf` x
    pong x        = write "PONG" (':' : drop 6 x)
    
getResponseNum :: String -> String
getResponseNum = takeWhile (/= ' ') . drop 1 . dropWhile (/= ' ')


eval :: String -> String -> String -> Net ()
eval x ch usr = eval' x ch usr cmdList

eval' :: String -> String -> String -> [(String, (String -> String -> String -> Net ()), String)] -> Net ()
eval' _ _  _   []     = return ()
eval' x ch usr (cmd:cmds) = if ((getCmdName cmd) `isPrefixOf` x)
                          then (getCmdFunction cmd) x ch usr
			  else eval' x ch usr cmds


privmsg :: String -> String -> Net ()
privmsg c s = write "PRIVMSG" (c ++ " :" ++ s)

write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t

uptime :: Net String
uptime = do
    now  <- io getClockTime
    zero <- asks starttime
    return . pretty $ diffClockTimes now zero

pretty :: TimeDiff -> String
pretty td = join . intersperse " " . filter (not . null) . map f $
    [(years          ,"y") ,(months `mod` 11,"m")
    ,(days   `mod` 18,"d") ,(hours  `mod` 24,"h")
    ,(mins   `mod` 60,"m") ,(secs   `mod` 60,"s")]
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

io :: IO a -> Net a
io = liftIO
