{-# LANGUAGE OverloadedStrings #-}

module SayPlugin
( plugin
) where

import qualified Data.Text as T(Text, isPrefixOf, words, unwords)

import IrcUtilities(Plugin(Plugin), Bot(Bot), IrcMsg(Privmsg), bNick, msgTo)
import MyUtils(when)
import Redirection(unwrapRedirectFromMsg)

-- Plugin Info
plugin :: Plugin
plugin = Plugin "SayPlugin" run helpAvailableUserCmds helpAvailableModCmds helpCmd

-- Main run
run :: IrcMsg -> Bot -> IO [T.Text]
run (Privmsg author channel message) bot@(Bot _ config _) = do
        when (",say" `T.isPrefixOf` message') $ do
                return [msgTo channel target textToSay]
        where
                (message', target) = unwrapRedirectFromMsg message author (bNick config)
                textToSay = T.unwords . drop 1 . T.words $ message'
run _ _ = return []

-- Help
helpAvailableUserCmds :: [T.Text]
helpAvailableUserCmds = ["say"]

helpAvailableModCmds :: [T.Text]
helpAvailableModCmds = []

helpCmd :: T.Text -> [T.Text]
helpCmd "say" = [",say <text> # Mówi <text>"]