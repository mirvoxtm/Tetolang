module Teto (runTeto, runImmediate, interactiveLoop, parseToString) where
import Prelude
import Data.List
import System.Environment (getArgs)
import Control.Monad (forever)
import Control.Exception (try, SomeException, evaluate)
import Control.DeepSeq (NFData(..), deepseq)
import Text.Megaparsec (parse, eof, errorBundlePretty)
import Parser.Parser (parseExpr, sc)
import Returns
import Eval (eval)

instance NFData Value where
    rnf (Numerical n)   = rnf n
    rnf (Booleanical b) = rnf b
    rnf (Vectorial xs)  = rnf xs
    rnf (Function a f)  = rnf a `seq` ()

runTeto :: IO ()
runTeto = do
    args <- getArgs
    if not (null args) && head args == "-f"
        then parseFile (unwords (tail args))
        else
            if not (null args) && head args == "-p"
            then runImmediate (unwords (tail args))
            else interactiveLoop

parseFile :: String -> IO ()
parseFile path
    | ".teto" `isSuffixOf` path = do
        input <- readFile path
        putStrLn (parseToString input)
    | otherwise = putStrLn "Invalid file extension"

parseToString :: String -> String
parseToString input =
    case parse (sc *> parseExpr <* eof) "" input of
        Left err   -> errorBundlePretty err
        Right expr -> show (eval expr)

runImmediate :: String -> IO ()
runImmediate input =
    case parse (sc *> parseExpr <* eof) "" input of
        Left err   -> putStrLn (errorBundlePretty err)
        Right expr -> do
            let val = eval expr
            result <- try (evaluate (val `deepseq` val)) :: IO (Either SomeException Value)
            case result of
                Left ex  -> putStrLn ("Error: " ++ show ex)
                Right v  -> print v

interactiveLoop :: IO ()
interactiveLoop =
    putStrLn "(--         Welcome to Teto         --)" >>
    putStrLn "(--  Press Ctrl-C to exit the REPL  --)" >>
    forever (do
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
        )