module DicePlugin
( plugin
) where

-- This plugin very needs refactorization in terms of evaluate-try-exceptions, it stinks

import Control.Monad(when)
import Control.Monad.Random(evalRandIO, getRandomR, RandomGen, Rand)
import Control.Exception(try, evaluate, SomeException)
import qualified Data.Text as T(Text, isPrefixOf, pack)

import IrcUtilities(IrcMsg(Privmsg), Bot(Bot), Plugin(Plugin), privmsgTo, bNick, readInt)
import Redirection(unwrapRedirectFromMsg)

-- Plugin Info
plugin :: Plugin
plugin = Plugin "DicePlugin" run helpAvailableUserCmds helpAvailableModCmds helpCmd

-- Main run
run :: IrcMsg -> Bot -> IO [T.Text]
run (Privmsg author channel message) bot@(Bot _ config _) = when (",dice" `T.isPrefixOf` message') $ do
        response <- parse message'
        msgTo channel target response
        where
                (message', target) = unwrapRedirectFromMsg message author (bNick config)
run _ _ = return []

parse :: T.Text -> IO T.Text
parse msg
        | argsCount == 1 = parse0
        | argsCount == 2 = parse1 (args !! 1)
        | argsCount == 3 = parse2 (args !! 1) (args !! 2)
        | otherwise = return "Nieprawidlowa liczba argumentow dla ,dice"
        where
                args = words msg
                argsCount = length args

-- It can be refactorized to one-liner
parse0 :: IO T.Text
parse0 = do
        value <- evalRandIO (dice 1 6)
        return $ (append "Wylosowana liczba (1-6) to " . T.pack . show) value

-- Code below, parse1 and parse2 needs refactorization
parse1 :: T.Text -> IO T.Text
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
        ",dice # Wyświetla losową liczbę pomiędzy 1 i 6",
        ",dice <n> # Wyświetla losową liczbę pomiędzy 1 i <n>",
        ",dice <min> <max> # Wyświetla losową liczbę pomiędzy <min> i <max>"
        ]