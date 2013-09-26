{-# LANGUAGE OverloadedStrings #-}

module QuitPlugin
( plugin
) where

import System.Exit(exitWith, ExitCode(ExitSuccess))
import qualified Data.Text as T(Text, isPrefixOf)

import IrcUtilities(Plugin(Plugin), IrcMsg(Privmsg), Bot(Bot), handle, isAdmin, nickFromAuthor, bNick)
import Redirection(unwrapRedirectFromMsg)
import MyUtils(when)

-- Plugin Info
plugin :: Plugin
plugin = Plugin "QuitPlugin" run helpAvailableUserCmds helpAvailableModCmds helpCmd

-- Main run
run :: IrcMsg -> Bot -> IO [T.Text]
run (Privmsg author _ message) bot@(Bot _ config _) = when (",quit" `T.isPrefixOf` message' && nick `isAdmin` config) $ return ["QUIT :Bye!"] >> exitWith ExitSuccess where
        nick = nickFromAuthor author
        (message', _) = unwrapRedirectFromMsg message author (bNick config)
run _ _ = return []

-- Help
helpAvailableUserCmds :: [T.Text]
helpAvailableUserCmds = []

helpAvailableModCmds :: [T.Text]
helpAvailableModCmds = ["quit"]

helpCmd :: T.Text -> [T.Text]
helpCmd "quit" = [",quit # Każe botu sobie pójść"]