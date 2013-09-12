module PluginsCore
( allPlugins
, enabledPlugins
) where

import IrcUtilities(Plugin(Plugin), Bot(Bot), BotConfig(BotConfig), name, bPlugins)
import Data.Map(fromList, Map, elems)

import qualified WhenPlugin
--import qualified IrcservPongPlugin
--import qualified DicePlugin
--import qualified GooglePlugin
--import qualified KarmaPlugin
--import qualified QuitPlugin

allPlugins :: Map String Plugin
--allPlugins = fromList $ map (\plugin -> (name plugin, plugin)) [WhenPlugin.plugin, IrcservPongPlugin.plugin, DicePlugin.plugin, GooglePlugin.plugin, KarmaPlugin.plugin, QuitPlugin.plugin]
allPlugins = fromList $ map (\plugin -> (name plugin, plugin)) [WhenPlugin.plugin]

enabledPlugins :: BotConfig -> [Plugin]
enabledPlugins config = filter (\p -> (name p) `elem` (bPlugins config)) (elems allPlugins)


