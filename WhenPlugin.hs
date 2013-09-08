module WhenPlugin
( plugin
) where

import Data.List(isPrefixOf)
import Control.Monad(when)
import Text.XML.HXT.Core((>>>), (/>), runX, getChildren, deep, hasAttrValue, readString, withParseHTML, withWarnings, getText, yes, no)
import Text.HandsomeSoup(css)
import Network.HTTP(simpleHTTP, getRequest, getResponseBody)

import IrcUtilities(Plugin(Plugin), Bot(Bot), IrcMsg(Privmsg), privmsgTo, bNick)
import Redirection(unwrapRedirectFromMsg)

-- Plugin Info
plugin :: Plugin
plugin = Plugin "WhenPlugin" run helpAvailableUserCmds helpAvailableModCmds helpCmd

spotkaniaUrl = "http://wiki.hswro.org/spotkania"

-- Main run
run :: IrcMsg -> Bot -> IO ()
run (Privmsg author channel message) bot@(Bot h config configDir) = when (",when" `isPrefixOf` message') $ do
        lastDate <- fetchLastDate
        privmsgTo h channel ("Najblizsze spotkanie odbedzie sie " ++ lastDate) target
        where
                (message', target) = unwrapRedirectFromMsg message author (bNick config)
run _ _ = return ()

fetchLastDate :: IO String
fetchLastDate = do
        response <- simpleHTTP (getRequest spotkaniaUrl)
        html <- getResponseBody response
        let doc = readString [withParseHTML yes, withWarnings no] html
        res <- runX $ doc >>> getChildren >>> deep (css "li") /> css "div" /> css "a" >>> hasAttrValue "title" (isPrefixOf "spotkania:") >>> getChildren >>> getText
        return (head res)


-- Help
helpAvailableUserCmds :: [String]
helpAvailableUserCmds = ["when"]

helpAvailableModCmds :: [String]
helpAvailableModCmds = []

helpCmd :: String -> [String]
helpCmd "when" = [",when # Wyświetla datę najbliższego spotkania"]