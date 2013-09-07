module IrcUtilities
( write
, privmsg
, privmsgTo
, readInt
, parseLine
, IrcMsg(Privmsg, Ping), author, channel, message, host, line
, BotConfig(BotConfig), bServer, bPort, bChannel, bNick
, Bot(Bot), handle, config, configDir
, Plugin(Plugin)
, nickFromAuthor
) where

import Network
import System.IO
import Text.Printf
import Data.List

data Plugin = Plugin
        { name :: String
        , run :: IrcMsg -> Bot -> IO ()
        , helpAvailableUserCmds :: [String]
        , helpAvailableModCmds :: [String]
        , helpCmd :: String -> [String]
        }

data BotConfig = BotConfig
        { bServer :: String
        , bPort :: Int
        , bChannel :: String
        , bNick :: String
        } deriving (Show, Read)

data Bot = Bot
        { handle :: Handle
        , config :: BotConfig
        , configDir :: FilePath
        }

data IrcMsg = Privmsg
        { author :: String
        , channel :: String
        , message :: String
        } | Ping
        { host :: String
        } | Othermsg
        { line :: String
        } deriving Show

write :: Handle -> String -> String -> IO ()
write h s t = do
        hPrintf h "%s %s\r\n" s t
        printf    "> %s %s\n" s t

privmsg :: Handle -> String -> String -> IO ()
privmsg h chan msg = write h "PRIVMSG" (chan ++ " :" ++ msg)

privmsgTo :: Handle -> String -> String -> Maybe String -> IO ()
privmsgTo h chan msg receiver = case receiver of 
        Just nick -> privmsg h chan (nick ++ ": " ++ msg)
        Nothing -> privmsg h chan msg

readInt :: String -> Int
readInt = read

nickFromAuthor :: String -> String
nickFromAuthor = fst . span (/= '!') . drop 1

parseLine :: String -> IrcMsg
parseLine line
        | "PING" == head splitLine = parseLinePing splitLine
        | length splitLine >= 4 && (splitLine !! 1) == "PRIVMSG" = parseLinePrivmsg splitLine
        | otherwise = Othermsg line
        where splitLine = words line

parseLinePing :: [String] -> IrcMsg
parseLinePing line = Ping (head . tail $ line)

parseLinePrivmsg :: [String] -> IrcMsg
parseLinePrivmsg line = Privmsg author channel message where
        (author:_:channel':rest) = line
        channel = if "#" `isPrefixOf` channel' then channel' else nickFromAuthor author
        message = drop 1 . unwords $ rest