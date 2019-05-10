import Data.Char

data LibraryInput = Exit | Error String | Book (String, String) | Author String | Title String

instance Show LibraryInput where
  show Exit = "Exit"
  show (Error xs) = "Invalid Input: " ++ xs
  show (Book (title, author)) = "Book: " ++ title ++ "; " ++ author
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
  -- replace with implementation:
  putStrLn "Welcome to your Library"
  library []
  putStrLn "Bye!"
  -- end replace

library :: [(String,String)] -> IO ()
library books =  do
  -- task c)
  -- replace with implementation:
  putStrLn ""
  libInput <- getInput
  handleLibraryInput books libInput
  -- end replace

getInput :: IO LibraryInput
getInput = do
  -- task b)
  putStrLn "Would you like to put back or take a book?\n Enter Book: Title's name; Author's name \nAre you looking for an author?\n Enter Author: Author's name \nAre you looking for a special book?\n Enter Title: Title's name."
  -- task b)
  -- replace with implementation:
  putStrLn ">"
  input <- getLine
  return (parseLibraryInput input)
  -- end replace



-- auxiliary functions:

handleLibraryInput :: [(String, String)] -> LibraryInput -> IO ()
handleLibraryInput books (Title title) = do
  putStrLn ("You have the following books with the title: " ++ title)
  putStr (showBooksWhich matchTitle books)
  library books
  where
    matchTitle (title', _) = title' == title
handleLibraryInput books (Author author) = do
  putStrLn ("You have the following books from " ++ author)
  putStr (showBooksWhich matchAuthor books)
  library books
  where
    matchAuthor (_, author') = author' == author
handleLibraryInput books (Book book) = do
  putStrLn "Do you want to (p)ut the book back or do you want to (t)ake the book?"
  cmd <- getLine
  newBooks <- handleBookCommand cmd
  library newBooks
  where
    handleBookCommand :: String -> IO [(String, String)]
    handleBookCommand "p" = putBook book books
    handleBookCommand "t" = takeBook book books
    handleBookCommand _ = do
      putStrLn "Wrong Input!"
      return books
handleLibraryInput books (Error xs) = do
  putStrLn ("There has been an error: " ++ xs)
  library books
handleLibraryInput books Exit = do
  return ()

showBooksWhich :: ((String,String) -> Bool) -> [(String,String)] -> String
showBooksWhich predicate
  = unlines
  . map (show . Book)
  . filter predicate

takeBook :: (String,String) -> [(String,String)] -> IO [(String,String)]
takeBook book books
  | elem book books = do
    putStrLn "Done!"
    return (filter (/=book) books)
  | otherwise = do
    putStrLn "You do not have this book!"
    return books

putBook :: (String,String) -> [(String,String)] -> IO [(String,String)]
putBook book books = do
  putStrLn "Done!"
  return (book : books)
