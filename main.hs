{-# LANGUAGE OverloadedStrings #-}

import Network(PortID(PortNumber), connectTo)
import System.IO(BufferMode(NoBuffering), hSetBuffering, hGetLine)
import Control.Monad(mapM)
import Data.Maybe(isJust, fromJust)
import Control.Monad.IO.Class(liftIO)
import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as B(unpack)
import Data.ByteString(ByteString)
import Control.Monad(when)
import qualified Data.Text as T(unpack)
import qualified Data.Text.Encoding as E(encodeUtf8, decodeUtf8)
import GHC.Conc(forkIO, threadDelay)
import Database.Redis(Connection, Redis, Reply, runRedis, lpush, connect, defaultConnectInfo, rpop)

import IrcUtilities(Bot(Bot), BotConfig(BotConfig), write, bNick, bChannel, bPort, bServer, run, parseLine)
import MyUtils(forever)
import PluginsCore(enabledPlugins)
import qualified HelpCore(run)
import qualified PongCore(run)

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
        write h $ "NICK " ++ (bNick config)
        write h $ "USER " ++ (bNick config) ++ " 0 * :tutorial bot"
        write h $ "JOIN " ++ (bChannel config)
        forkIO $ tell (Bot h config configDir)
        listen (Bot h config configDir)

listen :: Bot -> IO ()
listen bot = do
        conn <- connect defaultConnectInfo
        runRedis conn $ forever $ listen_loop bot conn

listen_loop :: Bot -> Connection -> Redis ()
listen_loop bot@(Bot h config _) conn = do
        msgs <- liftIO $ do
                ircMsg <- fmap parseLine $ hGetLine h
                --putStrLn $ show ircMsg
                results' <- sequence [HelpCore.run ircMsg bot, PongCore.run ircMsg bot]
                results <- mapM (\p -> (run p) ircMsg bot) $ enabledPlugins config
                return $ map E.encodeUtf8 $ concat $ results ++ results'
        lpush "ircQueue" msgs
        return ()

tell :: Bot -> IO ()
tell bot = do
        conn <- connect defaultConnectInfo
        runRedis conn $ forever $ tell_loop bot conn

tell_loop :: Bot -> Connection -> Redis ()
tell_loop bot@(Bot h config _) conn = do
        liftIO $ threadDelay 200000
        eitherMsg <- rpop "ircQueue"
        liftIO $ do
                msg <- unwrapMsg eitherMsg
                when (isJust msg) $ write h $ (T.unpack . E.decodeUtf8 . fromJust) msg
        return ()

unwrapMsg :: Either (Reply) (Maybe ByteString) -> IO (Maybe ByteString)
unwrapMsg msg = case msg of
        Right maybeMsg -> case maybeMsg of
                Just msg -> return $ Just msg
                Nothing -> return Nothing
        Left reply -> return Nothing
