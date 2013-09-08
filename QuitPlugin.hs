module QuitPlugin
( plugin
) where

import System.Exit(exitWith, ExitCode(ExitSuccess))
import Data.List(isPrefixOf)
import Control.Monad(when)

import IrcUtilities(Plugin(Plugin), IrcMsg(Privmsg), Bot(Bot), handle, write, isAdmin, nickFromAuthor)

-- Plugin Info
plugin :: Plugin
plugin = Plugin "QuitPlugin" run helpAvailableUserCmds helpAvailableModCmds helpCmd

-- Main run
run :: IrcMsg -> Bot -> IO ()
run (Privmsg author _ message) bot@(Bot h config _) = when (",quit" `isPrefixOf` message && nick `isAdmin` config) $ write h "QUIT" ":Bye!" >> exitWith ExitSuccess where
        nick = nickFromAuthor author
run _ _ = return ()

-- Help
helpAvailableUserCmds :: [String]
helpAvailableUserCmds = []

helpAvailableModCmds :: [String]
helpAvailableModCmds = ["quit"]

helpCmd :: String -> [String]
helpCmd "quit" = [",quit # Każe botu sobie pójść"]