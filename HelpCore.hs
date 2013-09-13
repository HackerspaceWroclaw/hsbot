{-# LANGUAGE OverloadedStrings #-}

module HelpCore
( run
) where

import Data.Maybe(fromJust)
import Data.List(find)
import qualified Data.Text as T(Text, intercalate, isPrefixOf, words, concat, unwords)

import IrcUtilities(IrcMsg(Privmsg), Bot(Bot), Plugin(Plugin), bNick, msgTo, helpAvailableModCmds, helpAvailableUserCmds, helpCmd)
import MyUtils(when)
import PluginsCore(enabledPlugins)
import Redirection(unwrapRedirectFromMsg)

run :: IrcMsg -> Bot -> IO [T.Text]
run (Privmsg author channel message) bot@(Bot h config _) = when (",help" `T.isPrefixOf` message') $ do
        if (length args <= 1) then
                return [msgTo channel target $ T.concat ["Komendy: ", T.unwords userCmds, " | Moderatorskie: ", T.unwords modCmds]]
        else
                return [msgTo channel target (T.intercalate " | " correctHelpCmd)]
        where
                (message', target) = unwrapRedirectFromMsg message author (bNick config)
                args = T.words message'
                searchCmd = args !! 1 -- cmd which we want help, is second arg: ",help <cmd>"
                enabledPlugins' = enabledPlugins config
                userCmds = concatMap helpAvailableUserCmds enabledPlugins'
                modCmds = concatMap helpAvailableModCmds enabledPlugins'
                correctHelpCmd :: [T.Text]
                correctHelpCmd
                        | searchCmd `elem` userCmds = helpCmd (fromJust $ find (\p -> searchCmd `elem` helpAvailableUserCmds p) enabledPlugins') searchCmd
                        | searchCmd `elem` modCmds = helpCmd (fromJust $ find (\p -> searchCmd `elem` helpAvailableModCmds p) enabledPlugins') searchCmd
                        | otherwise = ["Nie znaleziono takiej komendy"]
run _ _ = return []