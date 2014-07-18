module Aux where

import System.Environment
import System.IO
import System.Directory

t :: String -> IO()
t file = do
  file_content <- readFile file
  writeFile "test_file" $ file_content  

-- Parse file lines, according to it's language,
-- removing tabs, new lines and commented lines,
-- and returns a list with the parsed lines.
parse_lines :: [String] -> String ->  [String]
parse_lines [] _ = []
parse_lines lines@(x:xs) lang
  | (is_new_line x) = parse_lines xs lang
  | (is_tab x) = parse_lines xs lang
  | (is_commented x lang) = parse_lines xs lang
  | (is_block x lang) = parse_lines(remove_block lines lang) lang
  | otherwise = x : parse_lines xs lang

-- Identifies the language of a file
file_lang :: String -> String
file_lang [] = ""
file_lang(_:".java") = "java"
file_lang(_:".c") = "C"
file_lang(_:".hs") = "haskell"
file_lang(_:".R") = "R"
file_lang(_:".py") = "python"
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

-- Returns if a given line is commented, i.e in C, by //
is_commented :: String -> String -> Bool
is_commented [] _ = False
is_commented ('-':'-':xs) "haskell" = True
is_commented('/':'/':xs) "C" = True
is_commented('/':'/':xs) "java" = True
is_commented('#':xs) "R" = True
is_commented('#':xs) "python" = True
is_commented (x:xs) lang
  | (x == '\t') || (x == ' ') = is_commented xs lang
  | otherwise = False

-- Returns if a given line is commented by comment block, i.e in C, /*
is_block :: String -> String -> Bool
is_block [] _ = False
is_block('/':'*': xs) "C" = True
is_block('/':'*': xs) "java" = True
is_block('{':'-': xs) "haskell" = True
is_block (x:xs) lang = is_block xs lang

-- Returns the file lines, without the comment block. 
remove_block :: [String] -> String -> [String]
remove_block [] _ = []
remove_block (x:xs) lang
  | (has_end_block x lang == True) = xs
  | otherwise = remove_block xs lang

-- Returns if a given line has the end block characters.
-- i.e in C, */
has_end_block :: String -> String -> Bool
has_end_block [] _ = False
has_end_block ('*':'/':xs) "C"  = True
has_end_block ('*':'/':xs) "java"  = True
has_end_block ('-':'}':xs) "haskell"  = True
has_end_block (x:xs) lang = has_end_block xs lang

-- Adds "_locc" to a file name
-- i.e "file.java" is "file_locc.java"
-- Used when the "--print" option is passed
-- to the program, to dump the parsed file in
-- a new one.(name returned by this function)
locc_file_name :: String -> String
locc_file_name [] = []
locc_file_name ('.':xs) = "_locc." ++ xs
locc_file_name (x:xs) = x : locc_file_name xs



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