import Network
import System.IO
import Text.Printf
import Data.List
import System.Exit
import Control.Monad(mapM)
import Data.Map(elems)
import System.Environment (getArgs)

import IrcUtilities
import PluginsCore(enabledPlugins)
import HelpCore(runHelp)

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
listen bot@(Bot h config _) = do
        t <- hGetLine h
        putStrLn t
        let ircMsg = parseLine t
        putStrLn $ show ircMsg
        HelpCore.runHelp ircMsg bot
        mapM (\p -> (run p) ircMsg bot) $ enabledPlugins config
        listen bot

