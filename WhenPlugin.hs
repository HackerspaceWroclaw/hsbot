{-# LANGUAGE OverloadedStrings #-}

module WhenPlugin
( plugin
) where

import Text.XML.HXT.Core((>>>), (/>), runX, getChildren, deep, hasAttrValue, readString, withParseHTML, withWarnings, getText, yes, no)
import Text.HandsomeSoup(css)
import Data.List(isPrefixOf)
import Network.HTTP(simpleHTTP, getRequest, getResponseBody)
import qualified Data.Text as T(Text, isPrefixOf, append, pack)

import IrcUtilities(Plugin(Plugin), Bot(Bot), IrcMsg(Privmsg), bNick, msgTo)
import MyUtils(when)
import Redirection(unwrapRedirectFromMsg)

-- Plugin Info
plugin :: Plugin
plugin = Plugin "WhenPlugin" run helpAvailableUserCmds helpAvailableModCmds helpCmd

spotkaniaUrl = "http://wiki.hswro.org/spotkania"

-- Main run
run :: IrcMsg -> Bot -> IO [T.Text]
run (Privmsg author channel message) bot@(Bot _ config _) = when (",when" `T.isPrefixOf` message') $ do
        lastDate <- fetchLastDate
        return [msgTo channel target $ T.append "Najbliższe spotkanie odbedzie sie " lastDate]
        where
                (message', target) = unwrapRedirectFromMsg message author (bNick config)
run _ _ = return []

-- Possible refactorization: Text.HandsomeSoup.fromUrl
fetchLastDate :: IO T.Text
fetchLastDate = do
        response <- simpleHTTP (getRequest spotkaniaUrl)
        html <- getResponseBody response
        let doc = readString [withParseHTML yes, withWarnings no] html
        res <- runX $ doc >>> getChildren >>> deep (css "li") /> css "div" /> css "a" >>> hasAttrValue "title" (isPrefixOf "spotkania:") >>> getChildren >>> getText
        return $ T.pack $ head res

-- Help
helpAvailableUserCmds :: [T.Text]
helpAvailableUserCmds = ["when"]

helpAvailableModCmds :: [T.Text]
helpAvailableModCmds = []

helpCmd :: T.Text -> [T.Text]
helpCmd "when" = [",when # Wyświetla datę najbliższego spotkania"]