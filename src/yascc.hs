{-
Recieves source files as argument, determines the language
and counts the number of lines of code, ignoring spaces/enter, tabs
and comments
-}

-- TODO: add "--dump" option, to print parsed files, to new files
-- TODO: change "--total" option, not to print loc per file, only per language
-- TODO: read files from directories
-- TODO: functions that return comment characters per language, instead of having them hard-coded more than once in functions(is_tab, etc)
-- TODO: clean and simplify(?) code

module Main( main ) where
import System.Environment
import System.IO
import System.Directory
import Aux


main = do  
   args <- getArgs
   parse_arg(args)

-- Initial parsing
parse_arg :: [String] -> IO()
parse_arg [] = putStrLn("No arguments")
parse_arg (["--total"]) = putStrLn("No arguments")
parse_arg args@(x:xs)
  | (x == "--total") = yascc (parse_files xs) [] True
  | otherwise = yascc (parse_files args) [] False


-- Removes files with no language specification
-- i.e yascc.hs is haskell but yascc is unknown
-- and therefore removed.
parse_files :: [String] -> [String]
parse_files [] = []
parse_files (x:xs)
--  | (doesDirectoryExist x) = [] --parse_files ((dirContents x) : xs)
  | (file_lang x == "") = parse_files xs
  | otherwise = x : parse_files xs

dirContents :: String -> [String]
dirContents dir = do
  names <- getDirectoryContents dir
  let properNames = filter (`not` [".", ".."]) dir
  return(properNames)

-- Main function
-- t_flag is used to signal if the user wants the total
-- number of LOC per language printed instead of per file
yascc :: [String] -> [(String, Int)] -> Bool -> IO()
yascc [] [] False = putStr("")
yascc [] langs True = print_total langs
yascc files@(x:xs) [] t_flag@(False) = do 
  file <- readFile x
  let n_lines = length(parse_lines (lines(file)) (file_lang(x))) 
  let msg = x ++ ": " ++  (show(n_lines)) ++ " lines."
  putStrLn(msg)
  yascc xs [] False
yascc files@(x:xs) langs t_flag@(True) = do -- --total option
  file <- readFile x
  let n_lines = length(parse_lines (lines(file)) (file_lang(x)))
  let upd_list = (add_lang_count (file_lang x) n_lines langs)
  yascc xs upd_list True 
  
-- For a given language, updates the total number of lines in the tuple List
-- maintained by the main function "yascc"
-- This list is constructed only when the option "--total" is passed
add_lang_count :: String -> Int -> [(String, Int)] -> [(String, Int)]
add_lang_count lang n_lines [] = [(lang, n_lines)]
add_lang_count lang n_lines (x:xs)
  | (fst(x) == lang) = updated_tuple : xs
  | otherwise = x : add_lang_count lang n_lines xs
                where 
                  updated_tuple = (fst(x), snd(x) + n_lines)

-- Prints all LOC read in the given file(s), per language.
print_total :: [(String, Int)] -> IO()
print_total [] = putStr("")
print_total (x:xs) = do
  putStrLn sep
  putStrLn(lang_total)
  print_total xs
    where
      sep = "------------------"
      lang_total = fst(x) ++ ": " ++ show(snd(x))
