{-# LANGUAGE OverloadedStrings #-}

module GooglePlugin
( plugin
) where

import Data.Aeson(FromJSON(parseJSON), (.:), Value(Object), decode)
import Network.HTTP.Conduit(HttpException, httpLbs, withManager, parseUrl, responseBody)
import Control.Applicative((<$>), (<*>))
import Control.Monad(mzero)
import Data.Maybe(isJust, fromJust)
import qualified Data.ByteString.Lazy as L(ByteString)
import qualified Control.Exception as E(catch)
import qualified Data.Text as T(Text, isPrefixOf, words, unwords, tail, unpack, null, concat)

import IrcUtilities(IrcMsg(Privmsg), Bot(Bot), Plugin(Plugin), msgTo, bNick)
import MyUtils(when)
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
run :: IrcMsg -> Bot -> IO [T.Text]
run (Privmsg author channel message) bot@(Bot _ config configDir) = when (",g " `T.isPrefixOf` message') $ do
        pluginConfig <- fetchConfig configDir
        if (T.null query) then
                return [msgTo channel target "Brak frazy do wyszukania"]
        else (do
                jsonResult <- fetchJsonString $ requestUrl pluginConfig query
                when (isJust jsonResult) $ do
                        fetchRes <- fetchResult . fromJust $ jsonResult
                        when (isJust fetchRes) $ return [msgTo channel target $ fromJust fetchRes])
        where
                (message', target) = unwrapRedirectFromMsg message author (bNick config)
                query = unwrapQuery message'
run _ _ = return []

-- Based on plugin config and a query, generates request url
requestUrl :: GooglePluginConfig -> T.Text -> String
requestUrl config query = apiUrl ++ "key=" ++ (apiKey config) ++ "&cx=" ++ (cxValue config) ++ "&q=" ++ (T.unpack query)

-- fetch json response from the server
fetchJsonString :: String -> IO (Maybe L.ByteString)
fetchJsonString reqUrl = do
        request <- parseUrl reqUrl
        E.catch (do
                        response <- withManager $ httpLbs request
                        return $ Just (responseBody response))
                (\e -> (putStrLn . show) (e :: HttpException) >> return Nothing)


-- retrieve google result from json
fetchResult :: L.ByteString -> IO (Maybe T.Text)
fetchResult jsonString = do
        let maybeGSearch = (decode jsonString) :: Maybe GSearch
        case maybeGSearch of
                Just gSearch -> return $ Just $ (formattedGItem . head . items) gSearch
                Nothing -> putStrLn "GooglePlugin: JSON Decoding error" >> return Nothing

-- Utils
formattedGItem :: GItem -> T.Text
formattedGItem (GItem title link snippet) = T.concat ["Google search: \"", title, "\" <", link, "> (", snippet, ")"]

unwrapQuery :: T.Text -> T.Text
unwrapQuery = T.unwords . tail . T.words

fetchConfig :: FilePath -> IO GooglePluginConfig
fetchConfig configDir = do
        contents <- readFile (configDir ++ "/GooglePlugin/config")
        return $ ((read contents) :: GooglePluginConfig)

-- Data structures for JSON parsing
data GSearch = GSearch
        { kind :: T.Text
        , items :: [GItem]
        , searchInformation :: GSearchInfo
        } deriving Show
data GItem = GItem
        { title :: T.Text
        , link :: T.Text
        , snippet :: T.Text
        } deriving Show
data GSearchInfo = GSearchInfo
        { formattedSearchTime :: T.Text
        , formattedTotalResults :: T.Text
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
helpAvailableUserCmds :: [T.Text]
helpAvailableUserCmds = ["g"]

helpAvailableModCmds :: [T.Text]
helpAvailableModCmds = []

helpCmd :: T.Text -> [T.Text]
helpCmd "g" = [",g <query> # Pobiera pierwszy wynik wyszukiwania <query> w google, razem z tytułem i opisem"]