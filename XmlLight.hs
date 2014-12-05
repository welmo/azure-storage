module XmlLight where

import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as Map
import Text.XML.Light (Content(Elem, Text, CRef), Element, CData)
import qualified Text.XML.Light as XML
import Text.XML.Light.Cursor (Cursor)
import Text.XML.Light.Lexer (XmlSource)
import qualified Text.XML.Light.Cursor as Cursor

content ::(Element -> a) -> (CData -> a) -> (String -> a) -> Content -> a
content f _ _ (Elem a) = f a
content _ f _ (Text a) = f a
content _ _ f (CRef a) = f a

errorCursor :: Cursor -> Bool
errorCursor
    = content
        ((== "Error") . XML.qName . XML.elName)
        (const False)
        (const False)
    . Cursor.current

element :: Content -> Maybe Element
element = content Just (const Nothing) (const Nothing)

elementName :: Element -> String
elementName = XML.qName . XML.elName

elementText :: Element -> String
elementText
    = content
        (\a -> error $ "This is not text: " ++ show a)
        XML.cdData
        (\a -> error $ "This is not text: " ++ show a)
    . head
    . XML.elContent

toMap :: [Content] -> Maybe (Map String String)
toMap
    = fmap Map.fromList
    . sequence
    . map (fmap f . element)
    . filter (content (const True) (const False) (const False))
  where
    f = (,) <$> elementName <*> elementText

data Error = Error
    { code :: String
    , message :: String
    , detail :: Map String String
    }
  deriving (Show)

toError :: Map String String -> Maybe Error
toError m = do
    cd <- Map.lookup "Code" m
    msg <- Map.lookup "Message" m
    return $ Error cd msg (Map.delete "Message" (Map.delete "Code" m))

azureError :: XmlSource s => s -> Maybe Error
azureError str = Cursor.fromForest (XML.parseXML str)
    >>= Cursor.findChild errorCursor
    >>= element . Cursor.current
    >>= toMap . XML.elContent
    >>= toError

main :: IO ()
main = do
    str <- readFile "autherror.xml"
    print $ azureError str
