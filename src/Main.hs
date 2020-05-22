module Main where

import qualified Network.Wreq as Wreq
import qualified Text.Feed.Import
import qualified Text.RSS.Syntax as RSS
import qualified Text.Regex as Regex
import qualified Data.Text as T
import qualified System.Command as Command

import qualified Config
import qualified Episode
import Episode (Episode(Episode))

import Control.Lens ((^.))
import Text.Feed.Types (Feed(RSSFeed))
import Text.Regex (Regex)
import Data.Monoid ((<>))
import Text.Read (readMaybe)
import Control.Monad (forM_)

main :: IO ()
main = do
  config <- Config.readConfig
  items <- fmap parseItem' <$> getItems "https://horriblesubs.info/rss.php?res=1080"
  let selected = selectEpisodes (Config.shows config) items
  forM_ selected $ \e -> do
    putStrLn $ "Downloading episode " <> show (Episode.num . snd $ e) <> " of " <> T.unpack (Episode.showName . snd $ e)
    let dir = T.unpack $ Config.download_dir config <> "/" <> fst e
    Command.command_ [] "/usr/bin/env" ["mkdir", "-p", dir]
    Command.command_ [Command.EchoStdout False]  (T.unpack $ Config.transmission_remote config)
      [ "--add"
      , T.unpack (Episode.url . snd $ e)
      , "--download-dir",
      dir
      ]

type Folder = T.Text
selectEpisodes :: [Config.TvShow] -> [Episode] -> [(Folder, Episode)]
selectEpisodes tvShows episodes = [(Config.folder t, e)| t <- tvShows, e <- episodes, Config.title t == Episode.showName e]

getItems :: String -> IO [RSS.RSSItem]
getItems url = do
  res <- Wreq.get url
  let body = res ^. Wreq.responseBody
  case (Text.Feed.Import.parseFeedSource body) of
    Just (RSSFeed feed) ->
      pure . RSS.rssItems . RSS.rssChannel $ feed

    Just _ ->
      error "Expected rss feed"

    Nothing ->
      error "Failed to parse feed"

titleRegex :: Regex
titleRegex = Regex.mkRegex "\\[HorribleSubs\\] (['@,!\\(\\).&A-z0-9 -]+) - ([0-9.]+) \\[[0-9]+p\\]\\.mkv"

parseItem :: RSS.RSSItem -> Maybe Episode
parseItem item = do
  title <- RSS.rssItemTitle item

  (showName, num) <-
    case Regex.matchRegex titleRegex (T.unpack title) of
      Just [showName, num] -> pure (showName, num)
      _ -> Nothing

  url <- RSS.rssItemLink item
  num' <- (readMaybe num :: Maybe Float)
  pure $ Episode { Episode.showName = T.pack showName, Episode.url = url, Episode.num = num' }

parseItem' :: RSS.RSSItem -> Episode
parseItem' item =
  case parseItem item of
    Just episode ->
      episode

    Nothing ->
      error $ "Failed to parse episode " <> show (RSS.rssItemTitle item)
