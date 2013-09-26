{-# LANGUAGE OverloadedStrings #-}

module KarmaPlugin
( plugin
) where

import Data.List(isPrefixOf, isInfixOf)
import Data.Maybe(fromMaybe, fromJust, isJust)
import qualified Data.Map as Map(fromList, toList, insertWith)
import qualified Data.ByteString as Str(readFile, writeFile)
import qualified Data.ByteString.Char8 as B(pack, unpack)
import qualified Data.Text as T(Text, breakOn, intercalate, isInfixOf, pack, isPrefixOf, concat, append, unpack)


import IrcUtilities(IrcMsg(Privmsg), Plugin(Plugin), Bot(Bot), msgTo, bNick)
import MyUtils(when)
import Redirection(unwrapRedirectFromMsg)

-- Plugin Info
plugin :: Plugin
plugin = Plugin "KarmaPlugin" run helpAvailableUserCmds helpAvailableModCmds helpCmd

-- Main run
run :: IrcMsg -> Bot -> IO [T.Text]
run ircMsg@(Privmsg author channel message) bot@(Bot h config configDir)
        | ",karma" `T.isPrefixOf` message' = listCommand ircMsg bot
        | isJust $ unwrapReceiver message' = incrementKarma ircMsg bot
        | otherwise = return []
        where
                (message', target) = unwrapRedirectFromMsg message author (bNick config)
run _ _ = return []

listCommand :: IrcMsg -> Bot -> IO [T.Text]
listCommand (Privmsg author channel message) bot@(Bot h config configDir) = do
        contents <- Str.readFile (configDir++"/KarmaPlugin/users_karma")
        let result = read (B.unpack contents) :: [(String, Integer)]
        let result' = T.intercalate ", " $ map userInfo result
        return [msgTo channel target (T.append "Karma: " result')]
        where
                (message', target) = unwrapRedirectFromMsg message author (bNick config)

incrementKarma :: IrcMsg -> Bot -> IO [T.Text]
incrementKarma (Privmsg author channel message) bot@(Bot h config configDir) = do
        contents <- Str.readFile $ usersKarmaFile configDir
        let userMap = Map.fromList (read $ B.unpack contents :: [(String, Integer)])
        let userMap' = Map.insertWith (\new old -> old + 1) (T.unpack receiver) 1 userMap
        Str.writeFile (usersKarmaFile configDir) $ B.pack . show . Map.toList $ userMap'
        return [msgTo channel target (T.append "Dodano punkt dla " receiver)]
        where
                (message', target) = unwrapRedirectFromMsg message author (bNick config)
                receiver = fromJust $ unwrapReceiver message'

userInfo :: (String, Integer) -> T.Text
userInfo (nick, count) = T.concat ["*", T.pack nick, "* ", T.pack (show count)]

unwrapReceiver :: T.Text -> Maybe T.Text
unwrapReceiver msg = case receiver of
        Just nick -> if " " `T.isInfixOf` nick then Nothing else Just nick
        Nothing -> Nothing
        where
                receiver = if "++" `T.isInfixOf` msg then Just (fst (T.breakOn "++" msg)) else Nothing

usersKarmaFile :: FilePath -> FilePath
usersKarmaFile configDir = configDir ++ "/KarmaPlugin/users_karma"

-- Help
helpAvailableUserCmds :: [T.Text]
helpAvailableUserCmds = ["karma"]

helpAvailableModCmds :: [T.Text]
helpAvailableModCmds = []

helpCmd :: T.Text -> [T.Text]
helpCmd "karma" = [",karma # Wyświetla listę ludzi i ich karmę"]