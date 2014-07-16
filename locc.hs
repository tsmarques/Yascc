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
parse_arg ("--total" : []) = putStrLn("No arguments")
parse_arg args@(x:xs)
  | (x == "--total") = locc xs [] True
  | otherwise = locc args [] False

locc :: [String] -> [(String, Int)] -> Bool -> IO()
locc [] langs t_flag
  | t_flag == False = putStr("")
  | otherwise = print_total_res langs 
locc (x:xs) langs t_flag = do 
  file <- readFile x
  let n_lines = count_loc(lines(file))
  let msg = x ++ ": " ++  (show(n_lines))
  putStrLn(msg)
  locc xs (add_lang_count (file_lang x) n_lines langs) t_flag -- update the number of lines for a given language and continue


-- For a given language, updates the total number of lines in the tuple List
-- maintained by the main function "locc"
-- This list is constructed in all cases, but only when the option "--total"
-- is given to the program, that it's printed out
add_lang_count :: String -> Int -> [(String, Int)] -> [(String, Int)]
add_lang_count lang n_lines [] = [(lang, n_lines)]
add_lang_count lang n_lines (x:xs)
  | (fst(x) == lang) = updated_tuple : xs
  | otherwise = x : add_lang_count lang n_lines xs
                where 
                  updated_tuple = (fst(x), snd(x) + n_lines)


-- Count the number of lines in a given file, according to it's programming
-- language. It ignores comments(statement or block) and new lines
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


-- Returns if a given character is a new line
is_new_line :: String -> Bool
is_new_line "" = True 
is_new_line _ = False

-- Returns if a given line has a simple comment(i.e in C, //)
is_commented :: String -> Bool
is_commented [] = False
is_commented('/':'/':xs) = True
is_commented(x:xs) = is_commented xs

-- Returns if in a given line, starts a comment block(i.e in C, /*)
is_block :: String -> Bool
is_block [] = False
is_block('/':'*': xs) = True
is_block (x:xs) = is_block xs

-- Returns the file lines, without the comment block. 
-- This assumes that after the end block, there's no code.
-- i.e in C, after */ comes a new line, and note code
remove_block :: [String] -> [String]
remove_block [] = []
remove_block(x:xs)
  | has_end_block x == True = xs
  | otherwise = remove_block xs

-- Returns if a given line has the end block characters.
-- i.e in C, */
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
  
print_total_res :: [(String, Int)] -> IO()
print_total_res [] = putStr("")
print_total_res (x:xs) = do
  putStrLn(lang_total)
  print_total_res xs
    where
      lang_total = fst(x) ++ ": " ++ show(snd(x)) ++ " lines."