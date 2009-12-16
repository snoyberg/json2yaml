{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
import System.Environment (getArgs)
import Data.Object.Json
import Data.Object.Yaml
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Attempt

instance ConvertSuccess JsonScalar YamlScalar where
    convertSuccess (JsonString bs) = YamlScalar bs StrTag Any
    convertSuccess (JsonNumber n) = YamlScalar (cs n) FloatTag Any
    convertSuccess (JsonBoolean True)  = YamlScalar (cs "y") BoolTag Any
    convertSuccess (JsonBoolean False) = YamlScalar (cs "n") BoolTag Any
    convertSuccess JsonNull = YamlScalar (cs "~") NullTag Any

instance ConvertAttempt JsonDoc YamlDoc where
    convertAttempt = fmap (cs . (convertObject :: JsonObject -> YamlObject))
                   . ca

main :: IO ()
main = do
    args <- getArgs
    when (length args > 2) $ error "Usage: json2yaml [in] [out]"
    let (input:output:_) = args ++ repeat "-"
    jsonDoc <-
        case input of
            "-" -> JsonDoc `fmap` BL.getContents
            _ -> readJsonDoc input
    yd <- fa $ ca jsonDoc
    case output of
        "-" -> B.putStr $ unYamlDoc yd
        _ -> writeYamlDoc output yd
