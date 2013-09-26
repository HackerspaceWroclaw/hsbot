{-# LANGUAGE OverloadedStrings #-}

module IrcUtilities
( write
, privmsg
, parseLine
, IrcMsg(Privmsg, Ping), author, channel, message, host, line
, BotConfig(BotConfig), bServer, bPort, bChannel, bNick, bRedisPort, bAdmins, bPlugins
, Bot(Bot), handle, config, configDir
, Plugin(Plugin), name, run, helpAvailableUserCmds, helpAvailableModCmds, helpCmd
, nickFromAuthor
, isAdmin
, msgTo
) where

import System.IO(Handle)
import Text.Printf(hPrintf, printf)
import Data.List(isPrefixOf)
import Data.Map(fromList)
import qualified Data.ByteString as BS(ByteString, concat)
import qualified Data.Text as T(Text, pack, concat)

data Plugin = Plugin
        { name :: String
        , run :: IrcMsg -> Bot -> IO [T.Text]
        , helpAvailableUserCmds :: [T.Text]
        , helpAvailableModCmds :: [T.Text]
        , helpCmd :: T.Text -> [T.Text]
        }

data BotConfig = BotConfig
        { bServer :: String
        , bPort :: Int
        , bChannel :: String
        , bNick :: String
        , bRedisPort :: Int
        , bAdmins :: [String]
        , bPlugins :: [String]
        } deriving (Show, Read)

data Bot = Bot
        { handle :: Handle
        , config :: BotConfig
        , configDir :: FilePath
        }

data IrcMsg = Privmsg
        { author :: String
        , channel :: String
        , message :: T.Text
        } | Ping
        { host :: String
        } | Othermsg
        { line :: String
        } deriving Show

write :: Handle -> String -> IO ()
write h s = do
        hPrintf h "%s\r\n" s
        printf    "> %s\n" s

privmsg :: String -> T.Text -> T.Text
privmsg chan msg = T.concat ["PRIVMSG ", T.pack chan, " :", msg]

msgTo :: String -> Maybe T.Text -> T.Text -> T.Text
msgTo chan receiver msg = case receiver of
        Just nick -> privmsg chan $ T.concat [nick, ": ", msg]
        Nothing -> privmsg chan msg

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
        message = T.pack . drop 1 . unwords $ rest

isAdmin :: String -> BotConfig -> Bool
isAdmin user config = user `elem` bAdmins config