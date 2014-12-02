module Cloud.Azure.Storage.Aeson
    ( azureOptions
    , azureErrorOptions
    ) where

import Data.Aeson.TH (defaultOptions, Options, fieldLabelModifier)
import Data.Char (toUpper)

azureLabelModifier :: String -> String
azureLabelModifier []     = []
azureLabelModifier (c:cs) = toUpper c:cs

azureOptions :: Options
azureOptions = defaultOptions
    { fieldLabelModifier = azureLabelModifier }

azureErrorLabelModifier :: String -> String
azureErrorLabelModifier [] = []
azureErrorLabelModifier str
    | str == "odataError" = "odata.error"
    | str == "errorValue" = "value"
    | otherwise           = str

azureErrorOptions :: Options
azureErrorOptions = defaultOptions
    { fieldLabelModifier = azureErrorLabelModifier }
