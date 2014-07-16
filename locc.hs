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
parse_arg (x:xs)
  | (x == "--total") = locc_sum(xs)
  | otherwise = locc(x:xs)

locc :: [String] -> IO()
locc [] = putStr("")
locc (x:xs) = do 
  file <- readFile x
  let n_lines = count_loc(lines(file))
  let msg = x ++ ": " ++  (show(n_lines))
  putStrLn(msg)
  locc xs
  
locc_sum :: [String] -> IO()
locc_sum (x:xs) = do 
  file <- readFile x
  let n_lines = count_loc(lines(file))
  let msg = x ++ ": " ++  (show(n_lines))
  putStrLn(msg)
  locc_sum xs
  
-- locc :: [String] -> IO()
-- locc [] = putStrLn("")
-- locc (x:xs) = do 
--   file <- readFile x
--   print_parsed_file(lines(file))

count_loc :: [String] ->  Int
count_loc [] = 0
count_loc (x:xs)
  | is_new_line x = count_loc xs
  | is_commented x = count_loc xs
  | is_block x = count_loc(remove_block(x:xs))
  | otherwise = 1 + count_loc xs
                
file_lang :: String -> String
file_lang [] = ""
file_lang(_:".java") = "java"
file_lang(_:".c") = "C"
file_lang(_:"hs") = "Haskell"
file_lang(x:xs) = file_lang xs

is_new_line :: String -> Bool
is_new_line "" = True 
is_new_line _ = False

is_commented :: String -> Bool
is_commented [] = False
is_commented('/':'/':xs) = True
is_commented(x:xs) = is_commented xs

is_block :: String -> Bool
is_block [] = False
is_block('/':'*': xs) = True
is_block (x:xs) = is_block xs

remove_block :: [String] -> [String]
remove_block [] = []
remove_block(x:xs)
  | has_end_block x == True = xs
  | otherwise = remove_block xs

has_end_block :: String -> Bool
has_end_block [] = False
has_end_block('*':'/':xs) = True
has_end_block(x:xs) = has_end_block xs

----------------- Debug functions ---------------------

print_parsed_file :: [String] ->  IO()
print_parsed_file [] = putStr ""
print_parsed_file (x:xs)
  | is_new_line x = print_parsed_file xs
  | is_commented x = print_parsed_file xs
  | is_block x = print_parsed_file(remove_block(x:xs))
  | otherwise = do
    putStrLn x
    print_parsed_file xs


print_lines :: [String] -> IO()
print_lines [] = putStrLn ""
print_lines (x:xs) = do
  putStrLn x
  print_lines xs