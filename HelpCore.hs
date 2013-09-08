module HelpCore
( runHelp
) where

import Data.String.Utils(join)
import Data.Maybe(fromJust)
import Data.List(isPrefixOf, find)
import Control.Monad(when)

import IrcUtilities(IrcMsg(Privmsg), Bot(Bot), Plugin(Plugin), bNick, privmsgTo, helpAvailableModCmds, helpAvailableUserCmds, helpCmd)
import PluginsCore(enabledPlugins)
import Redirection(unwrapRedirectFromMsg)

runHelp :: IrcMsg -> Bot -> IO ()
runHelp (Privmsg author channel message) bot@(Bot h config _) = when (",help" `isPrefixOf` message') $ do
        if (length args <= 1) then
                privmsgTo h channel ("Komendy: " ++ (unwords userCmds) ++ " | Moderatorskie: " ++ (unwords modCmds)) target
        else (do
                privmsgTo h channel (join " | " correctHelpCmd) target)
        where
                (message', target) = unwrapRedirectFromMsg message author (bNick config)
                args = words message'
                searchCmd = args !! 1
                enabledPlugins' = enabledPlugins config
                userCmds = concat $ map helpAvailableUserCmds enabledPlugins'
                modCmds = concat $ map helpAvailableModCmds enabledPlugins'
                correctHelpCmd :: [String]
                correctHelpCmd
                        | searchCmd `elem` userCmds = helpCmd (fromJust $ find (\p -> searchCmd `elem` helpAvailableUserCmds p) enabledPlugins') searchCmd
                        | searchCmd `elem` modCmds = helpCmd (fromJust $ find (\p -> searchCmd `elem` helpAvailableModCmds p) enabledPlugins') searchCmd
                        | otherwise = ["Nie znaleziono takiej komendy"]
runHelp _ _ = return ()