{-# LANGUAGE OverloadedStrings #-}

module GooglePlugin
( plugin
) where

import Data.List(isPrefixOf)
import Data.Aeson(FromJSON(parseJSON), (.:), Value(Object), decode)
import Network.HTTP.Conduit(HttpException, httpLbs, withManager, parseUrl, responseBody)
import Control.Applicative((<$>), (<*>))
import Control.Monad(mzero, when)
import qualified Data.ByteString.Lazy as L(ByteString)
import qualified Control.Exception as E(catch)

import IrcUtilities(IrcMsg(Privmsg), Bot(Bot), Plugin(Plugin), privmsgTo, bNick)
import Redirection(unwrapRedirectFromMsg)

-- Plugin Config
plugin :: Plugin
plugin = Plugin "GooglePlugin" run helpAvailableUserCmds helpAvailableModCmds helpCmd

apiUrl = "https://www.googleapis.com/customsearch/v1?"

data GooglePluginConfig = GooglePluginConfig
        { apiKey :: String
        , cxValue :: String
        } deriving (Show, Read)

-- Main run
run :: IrcMsg -> Bot -> IO ()
run (Privmsg author channel message) bot@(Bot h config configDir) = when (",g " `isPrefixOf` message') $ do
        pluginConfig <- fetchConfig configDir
        if (null query) then
                privmsgTo h channel "Brak frazy do wyszukania" target
        else (do
        jsonResult <- fetchJsonString $ requestUrl pluginConfig query
        case jsonResult of
                Right jsonString -> do
                        fetchRes <- fetchResult jsonString
                        case fetchRes of 
                                Right str -> privmsgTo h channel str target
                                Left error -> putStrLn error
                Left error -> putStrLn error)
        where
                (message', target) = unwrapRedirectFromMsg message author (bNick config)
                query = unwrapQuery message'
run _ _ = return ()

-- Based on plugin config and a query, generates request url
requestUrl :: GooglePluginConfig -> String -> String
requestUrl config query = apiUrl ++ "key=" ++ (apiKey config) ++ "&cx=" ++ (cxValue config) ++ "&q=" ++ query

-- fetch json response from the server
fetchJsonString :: String -> IO (Either String L.ByteString)
fetchJsonString reqUrl = do
        request <- parseUrl reqUrl
        E.catch (do
                        response <- withManager $ httpLbs request
                        return $ Right (responseBody response))
                (\e -> return $ Left (show (e :: HttpException)) )


-- retrieve result from json
fetchResult :: L.ByteString -> IO (Either String String)
fetchResult jsonString = do
        let maybeGSearch = (decode jsonString) :: Maybe GSearch
        case maybeGSearch of
                Just gSearch -> return $ Right ( formattedGItem $ (head . items) gSearch)
                Nothing -> return $ Left "JSon decoding error"

-- Utils
formattedGItem :: GItem -> String
formattedGItem (GItem title link snippet) = "Google search: \"" ++ title ++ "\" <" ++ link ++ "> (" ++ snippet ++ ")"

unwrapQuery :: String -> String
unwrapQuery = unwords . tail . words

fetchConfig :: FilePath -> IO GooglePluginConfig
fetchConfig configDir = do
        contents <- readFile (configDir ++ "/GooglePlugin/config")
        return $ ((read contents) :: GooglePluginConfig)

-- Data structures for JSON parsing
data GSearch = GSearch
        { kind :: String
        , items :: [GItem]
        , searchInformation :: GSearchInfo
        } deriving Show
data GItem = GItem
        { title :: String
        , link :: String
        , snippet :: String
        } deriving Show
data GSearchInfo = GSearchInfo
        { formattedSearchTime :: String
        , formattedTotalResults :: String
        } deriving Show

instance FromJSON GSearch where
        parseJSON (Object v) = GSearch <$>
                v .: "kind" <*>
                v .: "items" <*>
                v .: "searchInformation"
        parseJSON _ = mzero

instance FromJSON GItem where
        parseJSON (Object v) = GItem <$>
                v .: "title" <*>
                v .: "link" <*>
                v .: "snippet"
        parseJSON _ = mzero

instance FromJSON GSearchInfo where
        parseJSON (Object v) = GSearchInfo <$>
                v .: "formattedSearchTime" <*>
                v .: "formattedTotalResults"
        parseJSON _ = mzero

-- Help
helpAvailableUserCmds :: [String]
helpAvailableUserCmds = ["g"]

helpAvailableModCmds :: [String]
helpAvailableModCmds = []

helpCmd :: String -> [String]
helpCmd "g" = [",g <query> # Pobiera pierwszy wynik wyszukiwania <query> w google, razem z tytułem i opisem"]