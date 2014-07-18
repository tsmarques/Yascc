module Main( main ) where {-
Recieves source files as argument, determines the language
and counts the number of lines of code, ignoring spaces/enter, tabs
and comments
-}

-- TODO: add "--print" option, to print parsed files, to new files

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
  | (x == "--total") = locc (parse_files xs) [] True
  | otherwise = locc (parse_files args) [] False


-- Removes files with no language specification
-- i.e locc.hs is haskell but locc is unknown
-- and therefore removed.
parse_files :: [String] -> [String]
parse_files [] = []
parse_files (x:xs)
  | (file_lang x == "") = parse_files xs
  | otherwise = x : parse_files xs

-- Main function
-- t_flag is used to signal if the user wants the total
-- number of LOC per language printed.
locc :: [String] -> [(String, Int)] -> Bool -> IO()
locc [] langs t_flag
  | (t_flag == False) = putStr("") -- no t_flag, program finishes
  | otherwise = do -- else, print LOC per language
    putStrLn ""
    print_total langs
locc files@(x:xs) langs t_flag = do 
  file <- readFile x
  let n_lines = length(parse_lines (lines(file)) (file_lang(x))) 
  let msg = x ++ ": " ++  (show(n_lines)) ++ " lines."
  putStrLn(msg)
  locc xs (add_lang_count (file_lang x) n_lines langs) t_flag -- update the number of lines for a given language and continue to the next file
  

-- For a given language, updates the total number of lines in the tuple List
-- maintained by the main function "locc"
-- This list is constructed in all cases, but only when the option "--total"
-- is passed to the program, that it's printed out
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
  putStrLn(lang_total)
  print_total xs
    where
      lang_total = fst(x) ++ ": " ++ show(snd(x)) ++ " lines."