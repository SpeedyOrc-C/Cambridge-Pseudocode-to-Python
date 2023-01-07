module Main (main) where

import CambridgePseudocodeToPython (translate)
import System.Environment (getArgs)
import Data.Maybe (fromJust, isNothing)

main :: IO ()
main = do
    args <- getArgs
    if null args then
        error "No input file specified."

    else do
        let inputPath = head $ filter (\s -> head s /= '-') args
        if "--help" `elem` args || "-h" `elem` args then
            putStrLn "Usage: campseudo-to-py <pseudocode_source_code>"
        else do

            let outputPath = inputPath ++ ".py"

            raw <- readFile inputPath
            let output = translate raw

            if isNothing output then
                error "Syntax error(s) exists."

            else do
                if "-e" `elem` args then
                    putStr $ fromJust output

                else do
                    putStrLn $ "Writing to " ++ outputPath
                    writeFile outputPath $ fromJust output
