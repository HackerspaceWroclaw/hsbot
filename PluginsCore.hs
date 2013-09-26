module PluginsCore
( allPlugins
, enabledPlugins
) where

import IrcUtilities(Plugin(Plugin), Bot(Bot), BotConfig(BotConfig), name, bPlugins)
import Data.Map(Map, fromList, elems)

import qualified WhenPlugin
import qualified SayPlugin
--import qualified DicePlugin
import qualified GooglePlugin
import qualified KarmaPlugin
import qualified QuitPlugin

allPlugins :: Map String Plugin
allPlugins = fromList $ map (\plugin -> (name plugin, plugin)) [WhenPlugin.plugin, SayPlugin.plugin, GooglePlugin.plugin, QuitPlugin.plugin, KarmaPlugin.plugin]

enabledPlugins :: BotConfig -> [Plugin]
enabledPlugins config = filter (\p -> (name p) `elem` (bPlugins config)) (elems allPlugins)


