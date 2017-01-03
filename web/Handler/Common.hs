-- | Common handler functions.
module Handler.Common where

import Data.FileEmbed (embedFile)
import Import
import Sheet

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = do cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
                 return $ TypedContent "image/x-icon"
                        $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")

-- { "id": 1, "title": "A title", "content": "The content" }
instance ToJSON JSONCellData where
    toJSON (JSONCellData value origin) = object
        [ "value"       .= value
        , "origin "     .= origin
        ]

instance ToJSON JSONSheet where
   toJSON sheet = Array $ fromList [Array $ fromList [toJSON jsonCell | jsonCell <-row ] | row <- sheet] 
     

mockSheet = readSheet [["=SUM(B1)","=SUM(A1)"],["0", "0"], ["0", "0"]]


getTestJsonR :: Handler Value
getTestJsonR = returnJson $ (toJSONData mockSheet)