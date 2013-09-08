module IrcservPongPlugin
( plugin
) where

import IrcUtilities(IrcMsg(Ping), Plugin(Plugin), Bot, write, handle)

-- Plugin Info
plugin :: Plugin
plugin = Plugin "IrcservPongPlugin" run helpAvailableUserCmds helpAvailableModCmds undefined

-- Main run
run :: IrcMsg -> Bot -> IO ()
run (Ping host) bot = write (handle bot) "PONG " host
run _ _ = return ()

-- Help
helpAvailableUserCmds :: [String]
helpAvailableUserCmds = []

helpAvailableModCmds :: [String]
helpAvailableModCmds = []