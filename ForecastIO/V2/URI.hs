
-- |
-- Module      : ForecastIO.V2.URI
-- Copyright   : (c) 2015 Devan Stormont
--
-- License     : BSD-style
-- Maintainer  : stormont@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- This module builds a URI endpoint which can be used to download
-- data from Forecast.io. 
--
-- For official documentation, see:
-- <https://developer.forecast.io/docs/v2 Forecast.io, version 2 >.
-- Please start with this documentation for information about the general
-- service; it's quite detailed.
--
-- The usage of this module is quite simple. For example, to get an
-- endpoint for London:
--
-- > buildUri $ mkForecastEndpoint { feCoord = GPS 51.5072 0.1275, feApiKey = "YOUR-API-KEY" }
--
-- General usage is to generate a URI of the form:
-- https:\/\/api.forecast.io\/forecast\/APIKEY\/LATITUDE,LONGITUDE
module ForecastIO.V2.URI
   ( Endpoint(..)
   , Exclude(..)
   , ForecastEndpoint(..)
   , GPS(..)
   , Units(..)
   , mkForecastEndpoint
   , mkGPS
   ) where

import qualified Data.Text as T
import ForecastIO.V2.Types


-- | A class that can generate a URI endpoint from an instance of type 'a'.
class Endpoint a where
  -- | Builds the URI from the 'a' instance.
  buildUri :: a -> T.Text


-- | A data type for defining a Forecast.io endpoint.
--
-- The supported list of languages is defined by the
-- <https://developer.forecast.io/docs/v2 forecast.io documentation >.
--
-- The @feExtend@ record, when set, will cause a more fine-grained
-- request URI for hourly weather forecast data to be generated.
data ForecastEndpoint =
  ForecastEndpoint
   { feCoord   :: GPS
   , feApiKey  :: T.Text
   , feUnits   :: Maybe Units
   , feExclude :: [Exclude]
   , feExtend  :: Bool
   , feLang    :: Maybe T.Text
   } deriving (Show,Read)


-- | A class that contains GPS data about a location.
data GPS =
   GPS
      { latitude  :: Double
      , longitude :: Double
      } deriving (Show,Read,Eq)


-- | Defines the units in which the data is returned in. 'Units_US'
-- is the default behavior when omitted.
data Units = Units_US
           | Units_SI
           | Units_CA
           | Units_UK
           | Units_Auto
           deriving (Show,Read,Eq)


-- | A definition for a data segment to exclude from the results. If
-- everything is excluded, little useful data will be returned.
data Exclude = Exclude_Currently
             | Exclude_Minutely
             | Exclude_Hourly
             | Exclude_Daily
             | Exclude_Alerts
             | Exclude_Flags
             deriving (Show,Read,Eq)


instance Endpoint ForecastEndpoint where
   buildUri fe =  T.pack
               $  "https://api.forecast.io/forecast"
               ++ "/" ++ T.unpack (feApiKey fe)
               ++ "/" ++ show (latitude $ feCoord fe)
               ++ "," ++ show (longitude $ feCoord fe)
               ++ parseOptions fe


-- | Generates a default 'ForecastEndpoint' object. At a minimum, you'll
-- want to override the 'feCoord' and 'feApiKey' records.
mkForecastEndpoint
  :: ForecastEndpoint
mkForecastEndpoint =
  ForecastEndpoint
    mkGPS
    T.empty
    Nothing
    []
    False
    Nothing


-- Generates a default 'GPS' object.
mkGPS
  :: GPS
mkGPS =
  GPS 0.0 0.0


-----------------------------------------------------------
-- INTERNAL
-----------------------------------------------------------


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
