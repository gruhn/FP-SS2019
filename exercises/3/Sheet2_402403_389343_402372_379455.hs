--Excercise 1

-- a)

collatz :: Int -> [Int] 
collatz x = iterate (\y -> if even y then div y 2 else (3*y+1)) x
    
    
total_stopping_time :: Int -> Int
total_stopping_time 1 = 3
total_stopping_time x = length (takeWhile (/= 1) (collatz x))

-- b)

check_collatz :: Int -> Bool
check_collatz 1 = True
check_collatz n = (total_stopping_time n)<maxBound && check_collatz (n-1)


--Excercise 2

-- a)

drop_mult :: Int -> [Int] -> [Int]
drop_mult x xs = [y | y <- xs , mod y x /= 0]

dropall :: [Int]-> [Int]
dropall (x:xs) = x : dropall (drop_mult x xs)

primes :: [Int]
primes = dropall [2 ..]

goldbach :: Int -> [(Int,Int)]
goldbach n = [(x,y)| even n, x<-takeWhile (<n) primes, y<-filter (>=x) (takeWhile (<n) primes), odd x, odd y, x+y == n]

-- b) 
--not allowed
range :: [a] -> Int -> Int -> [a]
range xs a b = [xs !! i| i <- [a .. b]]


--Excercise 3 

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
      

getInput :: IO LibraryInput
getInput = do
  -- task b)
  putStrLn "Would you like to put back or take a book?\n Enter Book: Title's name; Author's name \nAre you looking for an author?\n Enter Author: Author's name \nAre you looking for a special book?\n Enter Title: Title's name."
  putChar '>'
  input <- getLine
  return (parseLibraryInput input)
  
--auxiliary functions 
getLibraryAction :: [(String,String)] -> LibraryInput -> IO()
getLibraryAction books Exit         = do return ()
getLibraryAction books (Error e)    = do putStrLn (show (Error e))
                                         library books
getLibraryAction books (Book (t,a)) = do putStrLn (show (Book (t,a))) 
                                         putStrLn "Do you want to (p)ut the book back or do you want to (t)ake the book?"
                                         input <- getLine
                                         evaluateAction input books (t,a)
getLibraryAction books (Author a)   = do putStrLn (show (Author a))
                                         putStrLn ("You have the following books from " ++ a)
                                         displayBooks_A a books
                                         library books
getLibraryAction books (Title t)    = do putStrLn (show (Title t))
                                         putStrLn ("You have the following books with the title: " ++ t)
                                         displayBooks_T t books
                                         library books
 
                                     
displayBooks_A :: String -> [(String,String)] -> IO()
displayBooks_A a [] = return()
displayBooks_A a ((title, author) :books) = if (a==author) then putStrLn (show (Book (title,author))) >> displayBooks_A a books else displayBooks_A a books
                                            
displayBooks_T :: String -> [(String,String)] -> IO()
displayBooks_T t [] = return()
displayBooks_T t ((title, author) :books) = if (t==title) then putStrLn (show (Book (title,author))) >> displayBooks_T t books else displayBooks_T t books
                                            
                                            
evaluateAction :: String -> [(String,String)] -> (String,String) -> IO()
evaluateAction "t" books b =  if (elem b books) then putStrLn "Done!" >> library (filter (/=b) books) else putStrLn "You do not have this book!" >> library books
evaluateAction "p" books b =  putStrLn "Done!" >> library (b:books)
evaluateAction _   books _ =  putStrLn "Wrong input!" >> library books

