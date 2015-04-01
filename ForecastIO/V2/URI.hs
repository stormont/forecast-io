
module ForecastIO.V2.URI
   ( Endpoint(..)
   , Exclude(..)
   , ForecastEndpoint(..)
   , GPS(..)
   , Units(..)
   ) where

import qualified Data.Text as T
import ForecastIO.V2.Types


{-
-- Base domain is: api.forecast.io
-- Usage: https://api.forecast.io/forecast/APIKEY/LATITUDE,LONGITUDE
-- Official documentation: https://developer.forecast.io/docs/v2
-}


class Endpoint a where
  buildUri :: a -> T.Text


data GPS =
   GPS
      { latitude  :: Double
      , longitude :: Double
      } deriving (Show,Read,Eq)


data Units = Units_US
           | Units_SI
           | Units_CA
           | Units_UK
           | Units_Auto
           deriving (Show,Read,Eq)


data Exclude = Exclude_Currently
             | Exclude_Minutely
             | Exclude_Hourly
             | Exclude_Daily
             | Exclude_Alerts
             | Exclude_Flags
             deriving (Show,Read,Eq)


data ForecastEndpoint =
  ForecastEndpoint
   { feCoord   :: GPS
   , feApi     :: T.Text
   , feUnits   :: Maybe Units
   , feExclude :: [Exclude]
   , feExtend  :: Bool
   , feLang    :: Maybe T.Text
   } deriving (Show,Read)


instance Endpoint ForecastEndpoint where
   buildUri fe =  T.pack
               $  "https://api.forecast.io/forecast"
               ++ "/" ++ T.unpack (feApi fe)
               ++ "/" ++ show (latitude $ feCoord fe)
               ++ "," ++ show (longitude $ feCoord fe)
               ++ parseOptions fe


parseOptions
  :: ForecastEndpoint
  -> String
parseOptions fe =
  case opts of
    [] -> ""
    xs -> "?" ++ (drop 1 $ concat xs)
  where opts = [ parseUnits (feUnits fe)
               , parseExcludes [] (feExclude fe)
               , parseExtend (feExtend fe)
               , parseLang (feLang fe)
               ]


parseUnits
  :: Maybe Units
  -> String
parseUnits Nothing           = ""
parseUnits (Just Units_US)   = "&units=us"
parseUnits (Just Units_SI)   = "&units=si"
parseUnits (Just Units_CA)   = "&units=ca"
parseUnits (Just Units_UK)   = "&units=uk"
parseUnits (Just Units_Auto) = "&units=auto"


parseExcludes
  :: [String]
  -> [Exclude]
  -> String
parseExcludes acc []     =
  if acc /= []
    then ("&exclude=" ++)
       $ drop 1
       $ concat acc
    else ""
parseExcludes acc (x:xs) =
  case x of
    Exclude_Currently -> parseExcludes (",currently" : acc) xs
    Exclude_Minutely  -> parseExcludes (",minutely" : acc)  xs
    Exclude_Hourly    -> parseExcludes (",hourly" : acc)    xs
    Exclude_Daily     -> parseExcludes (",daily" : acc)     xs
    Exclude_Alerts    -> parseExcludes (",alerts" : acc)    xs
    Exclude_Flags     -> parseExcludes (",flags" : acc)     xs


parseExtend
  :: Bool
  -> String
parseExtend False = ""
parseExtend True  = "&extend=hourly"


parseLang
  :: Maybe T.Text
  -> String
parseLang Nothing  = ""
parseLang (Just x) =
  if T.length x > 0
    then "&lang=" ++ T.unpack x
    else ""
