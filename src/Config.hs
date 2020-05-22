module Config where

import qualified System.Directory as Directory
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as LBS

import Data.Text (Text)
import Data.Aeson (FromJSON, (.:))
import GHC.Generics (Generic)
import Control.Applicative ((<|>))

data TvShow
  = TvShow { title :: Text, folder :: Text }
  deriving stock Generic

parseTitle :: Aeson.Value -> Aeson.Types.Parser TvShow
parseTitle = Aeson.withText "Episode Title" $ \t ->
    pure $ TvShow t t

parseFull :: Aeson.Value -> Aeson.Types.Parser TvShow
parseFull = Aeson.withObject "Episode" $ \o ->
    TvShow
        <$> o .: "title"
        <*> o .: "folder"

instance FromJSON TvShow where
  parseJSON v = parseTitle v <|> parseFull v

data Config
  = Config
    { download_dir :: Text
    , transmission_remote :: Text
    , mkdir :: Text
    , shows :: [TvShow]
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
