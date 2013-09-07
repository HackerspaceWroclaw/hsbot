module DicePlugin
( plugin
) where

--import Network
--import System.IO
--import Data.List
--import Control.Monad
--import Control.Monad.Random
--import Control.Exception
--import System.IO.Error

--import IrcUtilities
import Redirection(unwrapRedirectFromMsg)

-- Plugin Info
plugin :: Plugin
plugin = Plugin "DicePlugin" run helpAvailableUserCmds helpAvailableModCmds helpCmd

-- Main run
run :: IrcMsg -> Bot -> IO ()
run (Privmsg author channel message) bot@(Bot h config configDir) = when (",dice" `isPrefixOf` message') $ do
        response <- parse message'
        privmsgTo h channel response target
        where
                (message', target) = unwrapRedirectFromMsg message author (bNick config)
run _ _ = return ()

parse :: String -> IO String
parse msg
        | argsCount == 1 = parse0
        | argsCount == 2 = parse1 (args !! 1)
        | argsCount == 3 = parse2 (args !! 1) (args !! 2)
        | otherwise = return "Nieprawidlowa liczba argumentow dla ,dice"
        where
                args = words msg
                argsCount = length args

parse0 :: IO String
parse0 = do
        value <- evalRandIO (dice 1 6)
        return $ "Wylosowana liczba (1-6) to " ++ show value

parse1 :: String -> IO String
parse1 arg1 = do
        max' <- (try . evaluate . readInt) arg1 :: IO (Either SomeException Int)
        case max' of
                Right max -> if max >= 1 then do
                                value <- evalRandIO (dice 1 max)
                                return ("Wylosowana liczba (1-" ++ show max ++ ") to " ++ show value)
                        else
                                return "Musisz podac liczbe wieksza od zera"
                Left e -> return "Jeden z argumentow nie jest liczba"

parse2 :: String -> String -> IO String
parse2 arg1 arg2 = do
        min' <- (try . evaluate . readInt) arg1 :: IO (Either SomeException Int)
        max' <- (try . evaluate . readInt) arg2 :: IO (Either SomeException Int)
        case (min', max') of
                (Right min, Right max) -> if min <= max then do
                                value <- evalRandIO (dice min max)
                                return ("Wylosowana liczba (" ++ show min ++ "-" ++ show max ++ ") to " ++ show value)
                        else
                                return "Musisz podac liczbe wieksza od zera"
                (_, _) -> return "Jeden z argumentow nie jest liczba"

dice :: (RandomGen g) => Int -> Int -> Rand g Int
dice x y = getRandomR (x,y)

-- Help
helpAvailableUserCmds :: [String]
helpAvailableUserCmds = ["dice"]

helpAvailableModCmds :: [String]
helpAvailableModCmds = []

helpCmd :: String -> [String]
helpCmd "dice" = [
        ",dice # Prints random number between 1 and 6",
        ",dice <n> # Prints random numbers between 1 and <n>",
        ",dice <min> <max> # Prints random numbers between <min> and <max>"
        ]