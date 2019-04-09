module Episode where

import Data.Text (Text)

data Episode
  = Episode
    { showName :: Text
    , num :: Float
    , url :: Text
    }
    deriving (Show)
