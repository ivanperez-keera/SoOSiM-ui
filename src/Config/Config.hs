module Config.Config where

import Control.Monad
import Data.Maybe
import Paths

readConfigFile :: IO Config
readConfigFile = do
  cfg <- fmap readConfig . readFile =<< getDataFileName "config"
  case cfg of
   Nothing -> return defaultConfig
   Just x  -> return x

type Config = ColorSpec

type Color4    = (Float, Float, Float, Float)
type ColorPair = (Color4, Color4)
type ColorSpec = (ColorPair, ColorPair, ColorPair, ColorPair)

defaultConfig :: Config
defaultConfig = ( ((0.1,0.5,0.9,1.0),(0.4,0.4,0.9,0.9)) -- Processing units
                , ((0.5,0.9,0.7,1.0),(0.5,0.9,0.9,1.0)) -- Active components
                , ((0.9,0.9,0.3,1.0),(0.9,0.9,0.5,1.0)) -- Waiting components
                , ((0.9,0.9,0.6,1.0),(0.9,0.9,0.9,1.0)) -- Idle components
                ) 

readConfig :: String -> Maybe Config
readConfig = readConfig_0_0_0_1

readConfig_0_0_0_1 :: String -> Maybe Config
readConfig_0_0_0_1 c = case parsed of
  [(_, _)] -> Just conf
  _        -> Nothing
 where parsed      = reads c
       [(conf, _)] = parsed
