module Main where

import PrologParser
import System.IO
import System.Environment


runParser :: String -> IO ()
runParser str =
  case parseString str of
    Left err -> print err
    Right r -> print r

parseFromFile :: FilePath -> IO ()
parseFromFile path = do
  input <- readFile path
  case parseString input of
    Left err -> do 
      print err
      writeFile (path ++ ".out") (show err)
    Right r -> do
      writeFile (path ++ ".out") (show r)

main :: IO ()
main = do
  (a:args) <- getArgs
  --args <- tail argss
  if not $ null args then
    case head args of
      ('-':'-':mode) -> 
         case mode of
            "atom" -> parseFromFileByMode (args !! 1) parseAtom
            "relation" -> parseFromFileByMode (args !! 1) parseRelationFull
            "module" -> parseFromFileByMode (args !! 1) parseModule
            "prog" -> parseFromFile (args !! 1)
            "typeexpr" -> parseFromFileByMode (args !! 1) parseArrow
            "type" -> parseFromFileByMode (args !! 1) parseTypeDef
            otherwise -> print $ "There isn't '" ++ mode ++ "' mode"
      otherwise -> parseFromFile $ head args
  else
    print "No arguments"
