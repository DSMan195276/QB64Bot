module Types ( Net
             , Bot(Bot, socket, starttime)
	     , IRCMessage(IRCMessage, fullText, command, responseNum, channel, user, semiText)
	     , IRCCommandName
	     , IRCCommandFunction
	     , IRCCommandDescription
	     , IRCCommand(IRCCommand, commandName, commandFunction, commandDescription)
             )
	     where

import Control.Monad.Reader
import System.Time
import System.IO

type Net = ReaderT Bot IO

data Bot = Bot { socket :: Handle
               , starttime :: ClockTime
               }

data IRCMessage = IRCMessage { fullText :: String
                             , command :: String
                             , responseNum :: String
                             , channel :: String
                             , user :: String
                             , semiText :: String
                             } 
			     deriving (Show)

type IRCCommandName = String
type IRCCommandFunction = IRCMessage -> Net ()
type IRCCommandDescription = String

data IRCCommand = IRCCommand { commandName :: IRCCommandName
                             , commandFunction :: IRCCommandFunction
                             , commandDescription :: IRCCommandDescription
                             }

