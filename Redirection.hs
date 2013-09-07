module Redirection
( unwrapRedirectFromMsg
) where

import Data.String.Utils
import Data.Maybe
import IrcUtilities

unwrapRedirectFromMsg :: String -> String -> String -> (String, Maybe String)
unwrapRedirectFromMsg msg author botNick = if isNothing receiver' then direct else pipe where
        direct@(rest, receiver) = directmsgRedirector msg author botNick
        pipe@(rest', receiver') = pipeRedirector rest

pipeRedirector :: String -> (String, Maybe String)
pipeRedirector msg = if ' ' `elem` receiver || null receiver then (msg, Nothing) else (afterMsg, Just receiver) where
        revMsg = reverse msg
        (target, rest) = if '|' `elem` revMsg then span (/= '|') revMsg else ("", revMsg)
        receiver = strip . reverse $ target
        afterMsg = reverse . lstrip . drop 1 $ rest

directmsgRedirector :: String -> String -> String -> (String, Maybe String)
directmsgRedirector msg author botNick = if receiver == botNick then (afterMsg, Just nick) else (msg, Nothing) where
        (target, rest) = if ':' `elem` msg then span (/= ':') msg else ("", msg)
        receiver = strip target
        afterMsg = lstrip . drop 1 $ rest
        nick = nickFromAuthor author
