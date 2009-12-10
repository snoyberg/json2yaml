{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
import System.Environment (getArgs)
import Text.Yaml
import Data.Object.JSON
import Data.Object.Yaml
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import Data.Convertible.Text
import Data.Attempt

showRational :: Rational -> B.ByteString
showRational = convertSuccess . show . (fromRational :: Rational -> Double)

instance ConvertSuccess JsonScalar Yaml where
    convertSuccess (JsonString bs) = Yaml bs StrTag Any
    convertSuccess (JsonNumber n) = Yaml (showRational n) FloatTag Any
    convertSuccess (JsonBoolean True)  = Yaml (B8.pack "y") BoolTag Any
    convertSuccess (JsonBoolean False) = Yaml (B8.pack "n") BoolTag Any
    convertSuccess JsonNull = Yaml (B8.pack "~") NullTag Any
instance ConvertSuccess B8.ByteString Yaml where
    convertSuccess bs = Yaml bs StrTag Any

instance ToObject (Object B8.ByteString JsonScalar) Yaml Yaml where
    toObject = mapKeysValues convertSuccess convertSuccess
instance FromObject (Object Yaml Yaml) B8.ByteString JsonScalar where
    fromObject = return . toObject

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
