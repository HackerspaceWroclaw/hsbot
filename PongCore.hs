{-# LANGUAGE OverloadedStrings #-}

module PongCore
( run
) where

import IrcUtilities(IrcMsg(Ping), Plugin(Plugin), Bot, write, handle)
import qualified Data.Text as T(Text)

run :: IrcMsg -> Bot -> IO [T.Text]
run (Ping host) bot = write (handle bot) ("PONG " ++ host) >> return []
run _ _ = return []