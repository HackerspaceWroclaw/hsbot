module KarmaPlugin
( plugin
) where

import Data.List(isPrefixOf, isInfixOf)
import Data.Maybe(fromMaybe, fromJust, isJust)
import qualified Data.Map as Map(fromList, toList, insertWith)
import qualified Data.ByteString as Str(readFile, writeFile)
import qualified Data.ByteString.Char8 as B(pack, unpack)
import Data.String.Utils(join, split)

import IrcUtilities(IrcMsg(Privmsg), Plugin(Plugin), Bot(Bot), privmsgTo, bNick)
import Redirection(unwrapRedirectFromMsg)

-- Plugin Info
plugin :: Plugin
plugin = Plugin "KarmaPlugin" run helpAvailableUserCmds helpAvailableModCmds helpCmd

-- Main run
run :: IrcMsg -> Bot -> IO ()
run ircMsg@(Privmsg author channel message) bot@(Bot h config configDir)
        | ",karma" `isPrefixOf` message' = listCommand ircMsg bot
        | isJust $ unwrapReceiver message' = incrementKarma ircMsg bot
        | otherwise = return ()
        where
                (message', target) = unwrapRedirectFromMsg message author (bNick config)
run _ _ = return ()

listCommand :: IrcMsg -> Bot -> IO ()
listCommand (Privmsg author channel message) bot@(Bot h config configDir) = do
        contents <- Str.readFile (configDir++"/KarmaPlugin/users_karma")
        let result = read (B.unpack contents) :: [(String, Integer)]
        let result' = join ", " $ map userInfo result
        privmsgTo h channel ("Karma: " ++ result') target
        where
                (message', target) = unwrapRedirectFromMsg message author (bNick config)

incrementKarma :: IrcMsg -> Bot -> IO ()
incrementKarma (Privmsg author channel message) bot@(Bot h config configDir) = do
        contents <- Str.readFile $ usersKarmaFile configDir
        let userMap = Map.fromList (read $ B.unpack contents :: [(String, Integer)])
        let userMap' = Map.insertWith (\new old -> old + 1) receiver 1 userMap
        Str.writeFile (usersKarmaFile configDir) $ B.pack . show . Map.toList $ userMap'
        privmsgTo h channel ("Dodano punkt dla " ++ receiver) target
        where
                (message', target) = unwrapRedirectFromMsg message author (bNick config)
                receiver = fromJust $ unwrapReceiver message'

userInfo :: (String, Integer) -> String
userInfo (nick, count) = "*" ++ nick ++ "* " ++ (show count)

unwrapReceiver :: String -> Maybe String
unwrapReceiver msg = if " " `isInfixOf` receiver || length splitted < 2 then Nothing else Just receiver where
        splitted = split "++" msg
        receiver = head splitted

usersKarmaFile :: FilePath -> FilePath
usersKarmaFile configDir = configDir ++ "/KarmaPlugin/users_karma"

-- Help
helpAvailableUserCmds :: [String]
helpAvailableUserCmds = ["karma"]

helpAvailableModCmds :: [String]
helpAvailableModCmds = []

helpCmd :: String -> [String]
helpCmd "karma" = [",karma # Wyświetla listę ludzi i ich karmę"]