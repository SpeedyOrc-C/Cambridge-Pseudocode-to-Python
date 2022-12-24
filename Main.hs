module Main (main) where

import MyParser (run)
import CambridgePseudocodeToPython ( cpFlowP, dump, initialState )

import Data.Maybe (fromJust, isJust)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    if null args then
        putStrLn "No input file specified."
    else do
        let inputPath = head args
            outputPath = inputPath ++ ".py"

        raw <- readFile inputPath
        let programMaybe = run cpFlowP raw

        if isJust programMaybe then do
            let program = snd $ fromJust programMaybe 
            let output = snd $ dump (initialState, program)
            writeFile outputPath output
            putStrLn $ "Complete. File generated at \"" ++ outputPath ++ "\""
        else do
            putStrLn "Syntax error(s) exists."
