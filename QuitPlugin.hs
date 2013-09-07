module QuitPlugin
( plugin
) where

import System.Exit(exitWith, ExitCode(ExitSuccess))
import Data.List(isPrefixOf)
import Control.Monad(when)

import IrcUtilities(Plugin(Plugin), IrcMsg(Privmsg), Bot, handle, write)

-- Plugin Info
plugin :: Plugin
plugin = Plugin "QuitPlugin" run helpAvailableUserCmds helpAvailableModCmds helpCmd

-- Main run
run :: IrcMsg -> Bot -> IO ()
run (Privmsg _ _ message) bot = when (",quit" `isPrefixOf` message) $ write (handle bot) "QUIT" ":Bye!" >> exitWith ExitSuccess
run _ _ = return ()

-- Help
helpAvailableUserCmds :: [String]
helpAvailableUserCmds = []

helpAvailableModCmds :: [String]
helpAvailableModCmds = ["quit"]

helpCmd :: String -> [String]
helpCmd "quit" = [",quit # Orders bot to exit the channel and stop."]