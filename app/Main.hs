{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main (main) where

import System.IO
import System.Environment
import System.Console.GetOpt
import Control.Monad
import Section3.Parser as S3
import Section3.Evaluator as S3
import Section3.Util as S3
import Section3.Wrong.Parser as S3W
import Section3.Wrong.Evaluator as S3W
import Section3.Wrong.Util as S3W
import Text.Parsec (ParseError)

data Options = Options
    { language :: Language
    , evaluation :: Evaluation
    , showHelpText :: Bool
    }

data Language
    = Section3
    | Section3Wrong

data Evaluation
    = OneShot
    | StepByStep

defaultOptions :: Options
defaultOptions = Options
    { language = Section3
    , evaluation = OneShot
    , showHelpText = False
    }

options :: [OptDescr (Options -> Either String Options)]
options =
    [ Option ['s'] ["show-steps"]
        (NoArg (\opts -> Right opts { evaluation = StepByStep }))
        "Print the evaluation process step by step."
    , Option ['t'] ["target"]
        (ReqArg (\l opts -> case l of
            "section3"       -> Right opts { language = Section3 }
            "section3-wrong" -> Right opts { language = Section3Wrong }
            _ -> Left $ "Invalid target language: " ++ l
        ) "LANG")
        $ unlines
            [ "Specify a target language. You can choose one from below:"
            , "  section3       - (Default) Untyped arithmetic expression language introduced in section 3"
            , "  section3-wrong - A variant of `section3` to handle the stuck by the \"wrong\" state"
            ]
    , Option ['h'] ["help"]
        (NoArg (\opts -> Right opts { showHelpText = True }))
        "Display this help text."
    ]

main :: IO ()
main = do
    args <- getArgs
    program <- getProgName
    case getOpt Permute options args of
        (opts, [], []) -> case foldM (flip id) defaultOptions opts of
            Right opts -> case opts of
                Options { showHelpText = True } -> putStrLn $ usage program
                Options { language = Section3, evaluation = OneShot } -> replEval S3.parse S3.eval S3.pprint
                Options { language = Section3, evaluation = StepByStep } -> replStep S3.parse S3.step S3.pprint
                Options { language = Section3Wrong, evaluation = OneShot } -> replEval S3W.parse S3W.eval S3W.pprint
                Options { language = Section3Wrong, evaluation = StepByStep } -> replStep S3W.parse S3W.step S3W.pprint
            Left error -> ioError (userError $ unlines [error, usage program])
        (_, _, errs) -> ioError (userError $ concat errs ++ usage program)

usage :: String -> String
usage program =
    let header = "Usage: " ++ program ++ " [OPTION...]"
    in  usageInfo header options


type Parser a = (String -> Either ParseError a)

type OneShotEvaluator a = (a -> a)

type StepByStepEvaluator a = (a -> Maybe a)

type PrettyPrinter a = a -> IO ()

replEval :: Parser a -> OneShotEvaluator a -> PrettyPrinter a -> IO ()
replEval parse eval pprint = repl $ \program ->
    case parse program of
        Right expr -> pprint $ eval expr
        Left error -> print error

replStep :: Parser a -> StepByStepEvaluator a -> PrettyPrinter a -> IO ()
replStep parse step pprint = repl $ \program ->
    case parse program of
        Right expr -> do
            putStr "   "
            pprint expr
            stepPrint expr
        Left error -> print error
    where
        stepPrint e = case step e of
            Just e' -> do
                putStr "-> "
                pprint e'
                stepPrint e'
            Nothing -> return ()

repl :: (String -> IO ()) -> IO ()
repl process = do
    input <- prompt "type> "
    case input of
        "" ->
            repl process
        ":q" ->
            putStrLn "Bye!"
        _ -> do
            process input
            repl process

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine
