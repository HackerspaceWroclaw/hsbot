import Network
import System.IO
import Text.Printf
import Data.List
import System.Exit
import System.Environment (getArgs)

import IrcUtilities
import qualified WhenPlugin
import qualified IrcservPongPlugin
import qualified DicePlugin
import qualified GooglePlugin
import qualified KarmaPlugin
import qualified QuitPlugin

allPlugins :: [(String, Plugin)]
allPlugins = [WhenPlugin.plugin, IrcservPongPlugin.plugin, DicePlugin.plugin, GooglePlugin.plugin, KarmaPlugin.plugin, QuitPlugin.plugin]
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
        t <- hGetLine $ handle bot
        putStrLn t
        let ircMsg = parseLine t
        putStrLn $ show ircMsg
        WhenPlugin.run ircMsg bot
        IrcservPongPlugin.run ircMsg bot
        DicePlugin.run ircMsg bot
        GooglePlugin.run ircMsg bot
        KarmaPlugin.run ircMsg bot
        QuitPlugin.run ircMsg bot
        listen bot

