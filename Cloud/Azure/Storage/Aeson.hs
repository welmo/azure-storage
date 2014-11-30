module Cloud.Azure.Storage.Aeson
    ( azureOptions
    ) where

import Data.Aeson.TH (defaultOptions, Options, fieldLabelModifier)
import Data.Char (toUpper)

azureOptions :: Options
azureOptions = defaultOptions
    { fieldLabelModifier = f }
  where
    f [] = []
    f (c:cs) = toUpper c:cs
