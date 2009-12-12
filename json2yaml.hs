{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
import System.Environment (getArgs)
import Text.Yaml
import Data.Object.Json
import Data.Object.Yaml
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import Data.Convertible.Text
import Data.Attempt

showRational :: Rational -> B.ByteString
showRational = convertSuccess . show . (fromRational :: Rational -> Double)

instance ConvertSuccess JsonScalar YamlScalar where
    convertSuccess (JsonString bs) = YamlScalar bs StrTag Any
    convertSuccess (JsonNumber n) = YamlScalar (showRational n) FloatTag Any
    convertSuccess (JsonBoolean True)  = YamlScalar (B8.pack "y") BoolTag Any
    convertSuccess (JsonBoolean False) = YamlScalar (B8.pack "n") BoolTag Any
    convertSuccess JsonNull = YamlScalar (B8.pack "~") NullTag Any
instance ConvertSuccess B8.ByteString YamlScalar where
    convertSuccess bs = YamlScalar bs StrTag Any
-- Why is the following required if it's in Data.Object.Dangerous?
instance (ConvertAttempt kIn kOut, ConvertAttempt vIn vOut)
    => FromObject (Object kOut vOut) kIn vIn where
    fromObject = mapKeysValuesM convertAttempt convertAttempt

main :: IO ()
main = do
    args <- getArgs
    when (length args > 2) $ error "Usage: json2yaml [in] [out]"
    let (input:output:_) = args ++ repeat "-"
    content <-
        case input of
            "-" -> BL.getContents
            _ -> BL.readFile input
    yo <- fa $ decode content
    case output of
        "-" -> B.putStr $ encodeYaml' (yo :: YamlObject)
        _ -> B.writeFile output $ encodeYaml' yo
