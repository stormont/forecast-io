{-# LANGUAGE TemplateHaskell #-}
module ForecastIO.V2.Types
  ( DataPoint(..)
  , DataBlock(..)
  , Alerts(..)
  , Flags(..)
  , Forecast(..)
  ) where

import Control.Applicative ((<*>), (<$>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text, pack)


{-
-- Base domain is: api.forecast.io
-- Usage: https://api.forecast.io/forecast/APIKEY/LATITUDE,LONGITUDE
-- Official documentation: https://developer.forecast.io/docs/v2
-}

-----------------------------------------------------------
-- EXPORTED
-----------------------------------------------------------


data DataPoint =
  DataPoint
    { dataPoint_time                        :: Maybe Int
    , dataPoint_summary                     :: Maybe Text
    , dataPoint_icon                        :: Maybe Text
    , dataPoint_sunriseTime                 :: Maybe Int
    , dataPoint_sunsetTime                  :: Maybe Int
    , dataPoint_moonPhase                   :: Maybe Float
    , dataPoint_moonPhaseError              :: Maybe Float
    , dataPoint_nearestStormDistance        :: Maybe Float
    , dataPoint_nearestStormDistanceError   :: Maybe Float
    , dataPoint_nearestStormBearing         :: Maybe Float
    , dataPoint_nearestStormBearingError    :: Maybe Float
    , dataPoint_precipIntensity             :: Maybe Float
    , dataPoint_precipIntensityError        :: Maybe Float
    , dataPoint_precipIntensityMax          :: Maybe Float
    , dataPoint_precipIntensityMaxError     :: Maybe Float
    , dataPoint_precipIntensityMaxTime      :: Maybe Int
    , dataPoint_precipProbability           :: Maybe Float
    , dataPoint_precipProbabilityError      :: Maybe Float
    , dataPoint_precipType                  :: Maybe Text
    , dataPoint_precipAccumulation          :: Maybe Float
    , dataPoint_precipAccumulationError     :: Maybe Float
    , dataPoint_temperature                 :: Maybe Float
    , dataPoint_temperatureError            :: Maybe Float
    , dataPoint_temperatureMin              :: Maybe Float
    , dataPoint_temperatureMinError         :: Maybe Float
    , dataPoint_temperatureMinTime          :: Maybe Int
    , dataPoint_temperatureMax              :: Maybe Float
    , dataPoint_temperatureMaxError         :: Maybe Float
    , dataPoint_temperatureMaxTime          :: Maybe Int
    , dataPoint_apparentTemperature         :: Maybe Float
    , dataPoint_apparentTemperatureError    :: Maybe Float
    , dataPoint_apparentTemperatureMin      :: Maybe Float
    , dataPoint_apparentTemperatureMinError :: Maybe Float
    , dataPoint_apparentTemperatureMinTime  :: Maybe Int
    , dataPoint_apparentTemperatureMax      :: Maybe Float
    , dataPoint_apparentTemperatureMaxError :: Maybe Float
    , dataPoint_apparentTemperatureMaxTime  :: Maybe Int
    , dataPoint_dewPoint                    :: Maybe Float
    , dataPoint_dewPointError               :: Maybe Float
    , dataPoint_windSpeed                   :: Maybe Float
    , dataPoint_windSpeedError              :: Maybe Float
    , dataPoint_windBearing                 :: Maybe Float
    , dataPoint_windBearingError            :: Maybe Float
    , dataPoint_cloudCover                  :: Maybe Float
    , dataPoint_cloudCoverError             :: Maybe Float
    , dataPoint_humidity                    :: Maybe Float
    , dataPoint_humidityError               :: Maybe Float
    , dataPoint_pressure                    :: Maybe Float
    , dataPoint_pressureError               :: Maybe Float
    , dataPoint_visibility                  :: Maybe Float
    , dataPoint_visibilityError             :: Maybe Float
    , dataPoint_ozone                       :: Maybe Float
    , dataPoint_ozoneError                  :: Maybe Float
    } deriving (Show,Read)


data DataBlock =
  DataBlock
    { dataBlock_summary :: Maybe Text
    , dataBlock_icon    :: Maybe Text
    , dataBlock_data    :: Maybe [DataPoint]
    } deriving (Show,Read)


data Alerts =
  Alerts
    { alerts_title       :: Maybe Text
    , alerts_expires     :: Maybe Int
    , alerts_description :: Maybe Text
    , alerts_uri         :: Maybe Text
    } deriving (Show,Read)


data Flags =
  Flags
    { flags_darksky_unavailable :: Maybe Text
    , flags_darksky_stations    :: Maybe [Text]
    , flags_datapoint_stations  :: Maybe [Text]
    , flags_isd_stations        :: Maybe [Text]
    , flags_lamp_stations       :: Maybe [Text]
    , flags_madis_stations      :: Maybe [Text]
    , flags_metar_stations      :: Maybe [Text]
    , flags_metno_license       :: Maybe Text
    , flags_sources             :: Maybe [Text]
    , flags_units               :: Maybe Text
    } deriving (Show,Read)


data Forecast =
  Forecast
    { forecast_latitude  :: Maybe Float
    , forecast_longitude :: Maybe Float
    , forecast_timezone  :: Maybe Text
    , forecast_offset    :: Maybe Float
    , forecast_currently :: Maybe DataPoint
    , forecast_minutely  :: Maybe DataBlock
    , forecast_hourly    :: Maybe DataBlock
    , forecast_daily     :: Maybe DataBlock
    , forecast_alerts    :: Maybe [Alerts]
    , forecast_flags     :: Maybe Flags
    } deriving (Show,Read)


-----------------------------------------------------------
-- INTERNAL
-----------------------------------------------------------


key_sources             = pack "sources"
key_isd_stations        = pack "isd-stations"
key_madis_stations      = pack "madis-stations"
key_metar_stations      = pack "metar-stations"
key_lamp_stations       = pack "lamp-stations"
key_datapoint_stations  = pack "datapoint-stations"
key_darksky_stations    = pack "darksky-stations"
key_darksky_unavailable = pack "darksky-unavailable"
key_metno_license       = pack "metno-license"
key_units               = pack "units"


instance FromJSON Flags where
   parseJSON (Object x) =   Flags
                        <$> (x .:? key_darksky_unavailable)
                        <*> (x .:? key_darksky_stations)
                        <*> (x .:? key_datapoint_stations)
                        <*> (x .:? key_isd_stations)
                        <*> (x .:? key_lamp_stations)
                        <*> (x .:? key_madis_stations)
                        <*> (x .:? key_metar_stations)
                        <*> (x .:? key_metno_license)
                        <*> (x .:? key_sources)
                        <*> (x .:? key_units)
   parseJSON _          = mzero


instance ToJSON Flags where
   toJSON x = object
      [ key_darksky_unavailable .= flags_darksky_unavailable x
      , key_darksky_stations    .= flags_darksky_stations    x
      , key_datapoint_stations  .= flags_datapoint_stations  x
      , key_isd_stations        .= flags_isd_stations        x
      , key_lamp_stations       .= flags_lamp_stations       x
      , key_madis_stations      .= flags_madis_stations      x
      , key_metar_stations      .= flags_metar_stations      x
      , key_metno_license       .= flags_metno_license       x
      , key_sources             .= flags_sources             x
      , key_units               .= flags_units               x
      ]


$(deriveJSON (defaultOptions { fieldLabelModifier = drop 10 }) ''DataBlock)
$(deriveJSON (defaultOptions { fieldLabelModifier = drop 10 }) ''DataPoint)
$(deriveJSON (defaultOptions { fieldLabelModifier = drop  7 }) ''Alerts)
$(deriveJSON (defaultOptions { fieldLabelModifier = drop  9 }) ''Forecast)
