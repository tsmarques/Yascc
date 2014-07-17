{-
Recieves source files as argument, determines the language
and counts the number of lines of code, ignoring spaces/enter
and comments
-}

module Main( main ) where
import System.Environment
import System.IO
import System.Directory


main = do  
   args <- getArgs
   parse_arg(args)

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
-- of LOC per language printed.
locc :: [String] -> [(String, Int)] -> Bool -> IO()
locc [] langs t_flag
  | (t_flag == False) = putStr("") -- program finishes
  | otherwise = do -- else, print LOC per language
    putStrLn ""
    print_total_res langs
locc (x:xs) langs t_flag = do 
  file <- readFile x
  let n_lines = count_loc(lines(file)) (file_lang x)
  let msg = x ++ ": " ++  (show(n_lines)) ++ " lines."
  putStrLn(msg)
  locc xs (add_lang_count (file_lang x) n_lines langs) t_flag -- update the number of lines for a given language and continue

-- Prints all LOC read in the given file(s), per language.
print_total_res :: [(String, Int)] -> IO()
print_total_res [] = putStr("")
print_total_res (x:xs) = do
  putStrLn(lang_total)
  print_total_res xs
    where
      lang_total = fst(x) ++ ": " ++ show(snd(x)) ++ " lines."


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


-- Count the number of lines in a given file, according to it's programming
-- language. It ignores comments(statement or block) and new lines
count_loc :: [String] -> String ->  Int
count_loc [] _ = 0
count_loc line@(x:xs) lang
  | (is_new_line x) = count_loc xs lang
  | (is_tab x) = count_loc xs lang
  | (is_commented x lang) = count_loc xs lang
  | (is_block x lang) = count_loc(remove_block line lang) lang
  | otherwise = 1 + count_loc xs lang

file_lang :: String -> String
file_lang [] = ""
file_lang(_:".java") = "java"
file_lang(_:".c") = "C"
file_lang(_:"hs") = "Haskell"
file_lang(x:xs) = file_lang xs

-- Returns if a given character is a new line
is_new_line :: String -> Bool
is_new_line "" = True 
is_new_line _ = False

-- Returns if a given character is a tab only or group of spaces
is_tab :: String -> Bool
is_tab [] = True
is_tab (x:xs)
  | (x == ' ') || (x == '\t') = is_tab xs
  | otherwise = False

-- Returns if a given line has a simple comment(i.e in C, //)
is_commented :: String -> String -> Bool
is_commented [] _ = False
is_commented ('-':'-':xs) "Haskell" = True
is_commented('/':'/':xs) "C" = True
is_commented('/':'/':xs) "java" = True
is_commented (x:xs) lang = is_commented xs lang

-- Returns if in a given line, starts a comment block(i.e in C, /*)
is_block :: String -> String -> Bool
is_block [] _ = False
is_block('/':'*': xs) "C" = True
is_block('/':'*': xs) "java" = True
is_block('{':'-': xs) "Haskell" = True
is_block (x:xs) lang = is_block xs lang

-- Returns the file lines, without the comment block. 
-- This assumes that after the end block, there's no code.
-- i.e in C, after */ comes a new line, and not code
-- The program "fails" if a line is something like:
-- /* comments */ while()... 
-- locc will assume that the whole line is commented and will output
-- a wrong number of LOC
remove_block :: [String] -> String -> [String]
remove_block [] _ = []
remove_block (x:xs) lang
  | has_end_block x lang == True = xs
  | otherwise = remove_block xs lang

-- Returns if a given line has the end block characters.
-- i.e in C, */
has_end_block :: String -> String -> Bool
has_end_block [] _ = False
has_end_block ('*':'/':xs) "C"  = True
has_end_block ('*':'/':xs) "java"  = True
has_end_block ('-':'}':xs) "Haskell"  = True
has_end_block (x:xs) lang = has_end_block xs lang

----------------- Debug functions ---------------------

-- Reads a file and prints it without comments, tabs and new lines
locc_print :: String -> String -> IO()
locc_print [] _ = putStrLn("")
locc_print x lang = do 
  file <- readFile x
  print_parsed_file (lines(file)) lang  

print_parsed_file :: [String] ->  String -> IO()
print_parsed_file [] _ = putStr ""
print_parsed_file (x:xs) lang
  | (is_new_line x) = print_parsed_file xs lang
  | (is_tab x) = print_parsed_file xs lang
  | (is_commented x lang) = print_parsed_file xs lang
  | (is_block x lang) = print_parsed_file (remove_block(x:xs) lang) lang
  | otherwise = do
    putStrLn x
    print_parsed_file xs lang

print_lines :: [String] -> IO()
print_lines [] = putStrLn ""
print_lines (x:xs) = do
  putStrLn x
  print_lines xs