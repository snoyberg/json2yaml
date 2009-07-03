import System.Environment (getArgs)
import Data.Yaml.Syck
import Text.JSON
import qualified System.IO.UTF8 as U
import Control.Monad
import Control.Applicative

toMonad :: Monad m => Result t -> m t
toMonad (Ok a) = return a
toMonad (Error s) = fail s

stringNode :: String -> YamlNode
stringNode = mkNode . EStr . packBuf

instance JSON YamlNode where
    readJSON JSNull = return $ mkNode ENil
    readJSON (JSBool True) = return $ stringNode "true"
    readJSON (JSBool False) = return $ stringNode "false"
    readJSON (JSString s) = return $ stringNode $ fromJSString s
    readJSON (JSArray vals) = mkNode . ESeq <$> mapM readJSON vals
    readJSON (JSObject o) = mkNode . EMap <$> mapM getPair (fromJSObject o)
    readJSON (JSRational b r) = return $ stringNode $ showJSRational' b r
                                       $ ""
    showJSON = undefined

getPair :: (String, JSValue) -> Result (YamlNode, YamlNode)
getPair (s, v) = do
    v' <- readJSON v
    return (stringNode s, v')

main :: IO ()
main = do
    args <- getArgs
    unless (length args == 2) $ error "Usage: json2yaml <in> <out>"
    let [input, output] = args
    content <- U.readFile input
    let yamlResult :: Result YamlNode
        yamlResult = decode content
    yaml <- toMonad yamlResult
    emitYamlFile output yaml
