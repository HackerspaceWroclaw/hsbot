{-# LANGUAGE OverloadedStrings #-}

import Network
import System.IO
import Text.Printf
import Data.List
import System.Exit
import Control.Monad(mapM)
import Control.Monad.IO.Class(liftIO)
import Data.Map(elems)
import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as B(pack, unpack)

import IrcUtilities
import PluginsCore(enabledPlugins)
import HelpCore(runHelp)
import Database.Redis(runRedis, lpush, connect, Connection, Redis, defaultConnectInfo)

main = do
        (config, configDir) <- fetchConfig
        runBot config configDir

fetchConfig :: IO (BotConfig, FilePath)
fetchConfig = do
        args <- getArgs
        let testFilePath = if null args then error "You didn't specified config dir" else head args
        contents <- readFile (testFilePath++"/bot_config")
        let config = (read contents) :: BotConfig
        return (config, testFilePath)

runBot :: BotConfig -> FilePath -> IO ()
runBot config configDir = do
        h <- connectTo (bServer config) (PortNumber (fromIntegral $ (bPort config)))
        hSetBuffering h NoBuffering
        write h "NICK" (bNick config)
        write h "USER" ((bNick config) ++ " 0 * :tutorial bot")
        write h "JOIN" (bChannel config)
        listen (Bot h config configDir)

listen :: Bot -> IO ()
listen bot = do
        conn <- connect defaultConnectInfo
        runRedis conn $ forever $ listen_loop bot conn

listen_loop :: Bot -> Connection -> Redis ()
listen_loop bot@(Bot h config _) conn = do
        msgs <- liftIO $ do
                t <- hGetLine h
                let ircMsg = parseLine t
                results <- mapM (\p -> (run p) ircMsg bot) $ enabledPlugins config
                return $ concat results
        lpush "ircQueue" msgs
        return ()


-- W pluginach chyba musi zostać to IO, ale możemy zmienić na IO [String], potem zbierać te stringi i robić concata
-- Poza tym to nie może być String tylko ten ByteString

        --hello <- get "hello"
        --world <- get "world"
        --liftIO $ print (hello,world)
--
--putStrLn t
--let ircMsg = parseLine t
--putStrLn $ show ircMsg
--HelpCore.runHelp ircMsg bot
--mapM (\p -> (run p) ircMsg bot) $ enabledPlugins config

