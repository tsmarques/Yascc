module Aux where

import System.Environment
import System.IO
import System.Directory

-- Parse file lines, according to it's language,
-- removing tabs, new lines and commented lines,
-- and returns a list with the parsed lines.
parse_lines :: [String] -> String ->  [String]
parse_lines [] _ = []
parse_lines lines@(x:xs) lang
  | (is_new_line x) = parse_lines xs lang
  | (is_tab x) = parse_lines xs lang
  | (is_commented x lang) = parse_lines xs lang
  | (has_block x lang) = parse_lines (remove_block lines lang) lang
  | otherwise = x : parse_lines xs lang

-- Identifies the language of a file
file_lang :: String -> String
file_lang [] = ""
file_lang (_:".java") = "java"
file_lang (_:".c") = "C"
file_lang (_:".hs") = "haskell"
file_lang (_:".R") = "R"
file_lang (_:".py") = "python"
file_lang (x:xs) = file_lang xs

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

-- Returns if a given line is commented, e.g in C, by //
-- There's a differnece between a line with a comment and
-- a commented line. This returns false if the line isn't
-- completely commented
-- e.g in C:
-- // this is a commented line
-- this is  a // line with a comment
is_commented :: String -> String -> Bool
is_commented [] _ = False
is_commented ('-':'-':xs) "haskell" = True
is_commented ('/':'/':xs) "C" = True
is_commented ('/':'/':xs) "java" = True
is_commented ('#':xs) "R" = True
is_commented ('#':xs) "python" = True
is_commented (x:xs) lang
  | (x == '\t') || (x == ' ') = is_commented xs lang
  | otherwise = False

-- Returns if a given line is commented by a comment block, e.g in C, /* */
has_block :: String -> String -> Bool
has_block [] _ = False
has_block ('/':'*': xs) "C" = True
has_block ('/':'*': xs) "java" = True
has_block ('{':'-': xs) "haskell" = True
has_block (x:xs) lang = has_block xs lang

-- Returns the file lines, without the comment block.
remove_block :: [String] -> String -> [String]
remove_block [] _ = []
remove_block (x:xs) lang
  | ((has_start_block x lang) && (has_end_block x lang)) = remove_start_block x lang : xs -- <code> /* this case*/
  | (has_start_block x lang) = (remove_start_block x lang) : (remove_block xs lang)
  | (has_end_block x lang) = (remove_end_block x lang) : xs
  | otherwise = remove_block xs lang
                
remove_start_block :: String -> String -> String
remove_start_block ('/':'*': xs) "C" = ""
remove_start_block ('/':'*': xs) "java" = ""
remove_start_block ('{':'-': xs) "haskell" = ""
remove_start_block ('*':'/': xs) "C" = xs
remove_start_block (x:xs) lang = x : (remove_start_block xs lang)

remove_end_block :: String -> String -> String
remove_end_block ('*':'/': xs) "C" = xs
remove_end_block ('*':'/': xs) "java" = xs
remove_end_block ('-':'}': xs) "haskell" = xs
remove_end_block (x:xs) lang = (remove_end_block xs lang)

has_start_block :: String -> String -> Bool
has_start_block [] _ = False
has_start_block ('/':'*':xs) "C"  = True
has_start_block ('/':'*':xs) "java"  = True
has_start_block ('{':'-':xs) "haskell"  = True
has_start_block (x:xs) lang = has_start_block xs lang

-- Returns if a given line has the end block characters.
-- e.g in C, */
has_end_block :: String -> String -> Bool
has_end_block [] _ = False
has_end_block ('*':'/':xs) "C"  = True
has_end_block ('*':'/':xs) "java"  = True
has_end_block ('-':'}':xs) "haskell"  = True
has_end_block (x:xs) lang = has_end_block xs lang

-- Adds "_yascc" to a file name
-- e.g for "file.java" returns "file_yascc.java"
-- Used when the "--print" option is passed
-- to the program, to dump the parsed file in
-- a new one.(name returned by this function)
yascc_file_name :: String -> String
yascc_file_name [] = []
yascc_file_name ('.':xs) = "_yascc." ++ xs
yascc_file_name (x:xs) = x : yascc_file_name xs



----------------- Debug functions ---------------------

-- Reads a file and prints it without comments, tabs and new lines
yascc_print :: String -> String -> IO()
yascc_print [] _ = putStrLn("")
yascc_print x lang = do 
  file <- readFile x
  print_parsed_file (lines(file)) lang  

print_parsed_file :: [String] ->  String -> IO()
print_parsed_file [] _ = putStr ""
print_parsed_file (x:xs) lang
  | (is_new_line x) = print_parsed_file xs lang
  | (is_tab x) = print_parsed_file xs lang
  | (is_commented x lang) = print_parsed_file xs lang
  | (has_block x lang) = print_parsed_file (remove_block(x:xs) lang) lang
  | otherwise = do
    putStrLn x
    print_parsed_file xs lang

print_lines :: [String] -> IO()
print_lines [] = putStrLn ""
print_lines (x:xs) = do
  putStrLn x
  print_lines xs
