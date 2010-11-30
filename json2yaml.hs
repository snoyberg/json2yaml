{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
import System.Environment (getArgs)
import Data.Object (Object)
import qualified Data.Object.Json as J
import qualified Data.Object.Yaml as Y
import Control.Monad (join, when)
import qualified Data.ByteString as S

main :: IO ()
main = do
    args <- getArgs
    when (length args > 2) $ error "Usage: json2yaml [in] [out]"
    let (input:output:_) = args ++ repeat "-"
    obj <- join $
        case input of
            "-" -> J.decode `fmap` S.getContents
            _ -> J.decodeFile input
    case output of
        "-" -> S.putStr $ Y.encode (obj :: Object S.ByteString S.ByteString)
        _ -> Y.encodeFile output obj
