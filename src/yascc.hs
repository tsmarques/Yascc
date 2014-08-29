{-
Recieves source files as argument, determines the language
and counts the number of lines of code, ignoring spaces/enter, tabs
and comments
-}

-- TODO: add "--dump" option, to print parsed files, to new files
-- TODO: clean and simplify(?) code

module Main( main ) where
import System.Environment -- used by getArgs
import System.IO
import System.Directory
import System.FilePath ((</>))
import Data.List
import Aux


main = do  
   args <- getArgs
   parse_arg args

-- Parsing of arguments
parse_arg :: [String] -> IO()
parse_arg [] = putStrLn("No arguments")
parse_arg (["--total"]) = putStrLn("No arguments")
parse_arg args@(x:xs)
  | (x == "--total") = do
    files <- parse_files xs
    yascc files [] True
  | otherwise = do
    files <- parse_files args
    yascc files [] False


-- Removes files with no language specification
-- i.e yascc.hs is haskell but yascc is unknown
-- and therefore removed.
-- If there's any directory  reads its content(files or folders)
parse_files :: [FilePath] -> IO[FilePath]
parse_files [] = return []
parse_files (x:xs) = do
  isDir <- doesDirectoryExist x
  if isDir then do
    tmp <- getCurrentDirectory
    content <- getAbsDirectoryContents x
    setCurrentDirectory x -- hack to make currDir and topDir correct.Not definitive
    currDir <- canonicalizePath "."
    topDir <- canonicalizePath ".."
    let filt_cont = filter(`notElem` [currDir, topDir]) content -- ignore "." and ".." folders
    setCurrentDirectory tmp
    parse_files (filt_cont ++ xs)
    else 
    if file_lang x == "" then parse_files xs -- file type no recognised
    else fmap (x:) (parse_files xs) -- recognises file type, then add it to list

-- I'm not the "author" of this function.
-- Found it in:
-- http://stackoverflow.com/questions/8572196/directory-contents-in-haskell
-- and was exactly what I needed but wasn't managing to code
-- This returns the absolute path of a file/directory
getAbsDirectoryContents :: FilePath -> IO [FilePath]
getAbsDirectoryContents dir = getDirectoryContents dir >>= mapM (canonicalizePath . (dir </>))

-- Main function
-- t_flag is used to signal if the user wants the total
-- number of LOC per language printed instead of per file
yascc :: [FilePath] -> [(String, Int)] -> Bool -> IO()
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
