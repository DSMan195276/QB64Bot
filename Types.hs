module Types ( Net
             , Bot(..)
             , IRCMessage(..)
             , IRCCommandName
             , IRCCommandFunction
             , IRCCommandDescription
             , IRCCommand(..)
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
                             , rightText :: String
                             , user :: String
                             , channel :: String
                             , semiText :: String
                             }
                             deriving (Show)

type IRCCommandName = String
type IRCCommandFunction = IRCMessage -> Net ()
type IRCCommandDescription = String

data IRCCommand = IRCCommand { name :: IRCCommandName
                             , function :: IRCCommandFunction
                             , description :: IRCCommandDescription
                             }

