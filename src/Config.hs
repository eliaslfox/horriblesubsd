module Config where

import qualified System.Directory as Directory
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS

import Data.Text (Text)
import Data.Aeson (FromJSON)
import GHC.Generics (Generic)

data Config 
  = Config
    { download_dir :: Text
    , transmission_remote :: Text
    , shows :: [Text]
    }
    deriving stock Generic
    deriving anyclass FromJSON

readConfig :: IO Config
readConfig = do
  configFile <- Directory.getXdgDirectory Directory.XdgConfig "horriblesubsd/config.json"
  config <- LBS.readFile configFile
  case Aeson.eitherDecode' config of
    Left err ->
      error err

    Right config ->
      pure config
