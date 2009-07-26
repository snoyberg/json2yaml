import System.Environment (getArgs)
import Text.Yaml hiding (decode)
import Text.JSON
import qualified System.IO.UTF8 as U
import Control.Monad

instance ToObject JSValue where
    toObject JSNull = toObject ""
    toObject (JSBool True) = toObject "true"
    toObject (JSBool False) = toObject "false"
    toObject (JSString s) = toObject $ fromJSString s
    toObject (JSArray vals) = toObject vals
    toObject (JSObject o) = toObject $ fromJSObject o
    toObject (JSRational b r) = toObject $ showJSRational' b r $ ""

toMonad :: Monad m => Result t -> m t
toMonad (Ok a) = return a
toMonad (Error s) = fail s

main :: IO ()
main = do
    args <- getArgs
    unless (length args == 2) $ error "Usage: json2yaml <in> <out>"
    let [input, output] = args
    content <- U.readFile input
    json <- toMonad $ decode content
    let obj = toObject (json :: JSValue)
    encodeFile output obj
