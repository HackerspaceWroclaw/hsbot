{-# LANGUAGE OverloadedStrings #-}

module Redirection
( unwrapRedirectFromMsg
) where

import Data.Maybe(isNothing, fromMaybe)
import IrcUtilities(nickFromAuthor)
import qualified Data.Text as T(Text, isInfixOf, breakOn, breakOnEnd, strip, drop, pack, null, stripSuffix, stripPrefix)

unwrapRedirectFromMsg :: T.Text -> String -> String -> (T.Text, Maybe T.Text)
unwrapRedirectFromMsg msg author botNick = if isNothing receiver' then direct else pipe where
        direct@(rest, receiver) = directmsgRedirector msg author botNick
        pipe@(rest', receiver') = pipeRedirector rest

pipeRedirector :: T.Text -> (T.Text, Maybe T.Text)
pipeRedirector msg = if " " `T.isInfixOf` receiver || T.null receiver then (msg, Nothing) else (afterMsg, Just receiver) where
        (rest, target) = if "|" `T.isInfixOf` msg then T.breakOnEnd "|" msg else (msg, "")
        receiver = T.strip target
        afterMsg = T.strip . fromMaybe rest . T.stripSuffix "|" $ rest

directmsgRedirector :: T.Text -> String -> String -> (T.Text, Maybe T.Text)
directmsgRedirector msg author botNick = if receiver == botNick' then (afterMsg, Just nick) else (msg, Nothing) where
        (target, rest) = if ":" `T.isInfixOf` msg then T.breakOn ":" msg else ("", msg)
        receiver = T.strip target
        botNick' = T.pack botNick
        afterMsg = T.strip . fromMaybe rest . T.stripPrefix ":" $ rest
        nick = T.pack . nickFromAuthor $ author
