{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
import System.Environment (getArgs)
import qualified Data.Aeson as J
import qualified Data.Yaml as Y
import Control.Monad (when)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Control.Applicative ((<$>))

main :: IO ()
main = do
    args <- getArgs
    when (length args > 2) $ error "Usage: json2yaml [in] [out]"
    let (input:output:_) = args ++ repeat "-"
    mval <- J.decode <$>
        case input of
            "-" -> L.getContents
            _ -> L.readFile input
    case mval of
        Nothing -> error "Invalid input JSON"
        Just val -> do
            case output of
                "-" -> S.putStr $ Y.encode (val :: Y.Value)
                _ -> Y.encodeFile output val
