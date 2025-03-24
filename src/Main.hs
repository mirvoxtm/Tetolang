{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
import Data.Maybe (fromMaybe)
import Parser.Parser
import Eval
import Control.Monad
import Returns
import Text.Megaparsec (parse, eof, errorBundlePretty)
import Control.Exception (try, SomeException, evaluate)
import Control.DeepSeq (deepseq, NFData(..))

instance NFData Value where
    rnf (Numerical n)   = rnf n
    rnf (Vectorial xs)  = rnf xs
    rnf (Function a f)  = rnf a `seq` () 

main :: IO ()
main = do
    forever $ do
        putStr "teto> "
        input <- getLine
        case parse (sc *> parseExpr <* eof) "" input of
            Left err   -> putStrLn (errorBundlePretty err)
            Right expr -> do
                let val = eval expr
                result <- try (evaluate (val `deepseq` val)) :: IO (Either SomeException Value)
                case result of
                    Left ex  -> putStrLn ("Error: " ++ show ex)
                    Right v  -> print v