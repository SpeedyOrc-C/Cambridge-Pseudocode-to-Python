module Main (main) where

import CambridgePseudocodeToPython (translate)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    if null args then
        putStrLn "No input file specified."

    else do
        let firstArg = head args
        if firstArg == "--help" || firstArg == "-h" then
            putStrLn "Usage: campseudo-to-py <pseudocode_source_code>"
        else do

            let inputPath = firstArg
                outputPath = inputPath ++ ".py"

            raw <- readFile inputPath
            let output = translate raw

            maybe
                (putStrLn "Syntax error(s) exists.")
                (writeFile outputPath)
                output