module Excercise3 where
    
import Data.Char

data LibraryInput = Exit | Error String | Book (String, String) | Author String | Title String

instance Show LibraryInput where
  show Exit = "Exit"
  show (Error xs) = "Invalid Input: " ++ xs
  show (Book (title, author)) = "Book: " ++ title ++ ";" ++ author
  show (Author author) = "Author: " ++ author
  show (Title title) = "Title: " ++ title

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace
  
parseLibraryInput :: String -> LibraryInput
parseLibraryInput input   | lhs == "Book" = Book ((trim (drop 1 title)), (trim (drop 1 author)))
                          | lhs == "Author" = Author (trim (drop 1 rhs))
                          | lhs == "Title" = Title (trim (drop 1 rhs))
                          | elem (map toLower input) ["q", "e", "exit", "quit"] = Exit
                          | otherwise = (Error input)
                            where
                              (lhs, rhs) = span (/= ':') (trim input)
                              (title, author) = span (/= ';') (trim rhs)
 
                            

-- exercise:
main :: IO ()
main = do
  -- task a)
  putStrLn "Welcome to your library"
  library []
  putStrLn "Bye!"
  return ()

library ::[(String,String)] -> IO ()
library books =  do
  -- task c)
  input <- getInput
  getLibraryAction books input
  return ()
      

getInput :: IO LibraryInput
getInput = do
  -- task b)
  putStrLn "Would you like to put back or take a book?\n Enter Book: Title's name; Author's name \nAre you looking for an author?\n Enter Author: Author's name \nAre you looking for a special book?\n Enter Title: Title's name."
  putChar '>'
  input <- getLine
  return (parseLibraryInput input)
  return Exit
  
--auxiliary functions 
getLibraryAction :: [(String,String)] -> LibraryInput -> IO()
getLibraryAction books Exit         = do return ()
getLibraryAction books (Error e)    = do putStrLn (show (Error e))
                                      >> library books
getLibraryAction books (Book (t,a)) = do putStrLn (show (Book (t,a))) 
                                      >> putStrLn "Do you want to (p)ut the book back or do you want to (t)ake the book?"
                                      -- TODO Error 
                                      >> evaluateAction getLine books (t,a)
getLibraryAction books (Author a)   = do putStrLn (show (Author a))
                                      >> putStrLn ("You have the following books from " ++ a)
                                      >> displayBooks_A a books
                                      >> library books
getLibraryAction books (Title t)    = do putStrLn (show (Title t))
                                      >> putStrLn ("You have the following books with the title: " ++ t)
                                      >> displayBooks_T t books
                                      >> library books
 
                                     
displayBooks_A :: String -> [(String,String)] -> IO()
displayBooks_A a [] = return()
displayBooks_A a ((title, author) :books) = if (a==author) then putStrLn (show (Book (title,author)))  else return ()
                                            >> displayBooks_A a books
                                            
displayBooks_T :: String -> [(String,String)] -> IO()
displayBooks_T t [] = return()
displayBooks_T t ((title, author) :books) = if (t==title) then putStrLn (show (Book (title,author)))  else return ()
                                            >> displayBooks_T t books
                                            
evaluateAction :: String -> [(String,String)] -> (String,String) -> IO()
evaluateAction "t" books b =  if (elem b books) then putStrLn "Done!" >> library (filter (/=b) books) else putStrLn "You do not have this book!" >> library books
evaluateAction "p" books b =  putStrLn "Done!" >> library (b:books)
evaluateAction _   books _ =  putStrLn "Wrong input!" >> library books

