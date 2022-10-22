import Polinomial
import Control.Monad (when)

main :: IO()
main = 
    do
        putStr "PFL Project 1 - Polinomials\n"          
        loop

        putStr "\nFinishing Program\n"
        return()



loop :: IO()
loop = 
    do
        putStr "\nWhat do you want to do:\n"
        putStr "1 - Normalize polinomial\n"
        putStr "2 - Sum two polinomials\n"
        putStr "3 - Multiply two polinomials\n"
        putStr "4 - derive a polinomial\n"
        putStr "Anything else - Quit\n"
        putStr "Choice: "
        choice <- getLine
        when (choice == "1") loop1
        when (choice == "2") loop2
        when (choice == "3") loop3
        when (choice == "4") loop4

loop1 :: IO()
loop1 = do
    putStr "Write a polinomial: "
    polinomial <- getLine
    putStrLn "Result: "
    putStrLn (normalize (parsePolinomial polinomial))
    loop

loop2 :: IO()
loop2 = do
    putStr "Write a polinomial: "
    polinomial1 <- getLine
    putStr "Write another polinomial: "
    polinomial2 <- getLine
    putStrLn "Result: "
    putStrLn (sumPolinomial (parsePolinomial polinomial1) (parsePolinomial polinomial2))
    loop

loop3 :: IO()
loop3 = do
    putStr "Write a polinomial: "
    polinomial1 <- getLine
    putStr "Write another polinomial: "
    polinomial2 <- getLine
    putStrLn "Result: "
    putStrLn (multiplyPolinomial (parsePolinomial polinomial1) (parsePolinomial polinomial2))
    loop

loop4 :: IO()
loop4 = do
    putStr "Write a polinomial: "
    polinomial <- getLine
    putStr "Write the symbol to be derived to (one char): "
    symbol <- getLine
    putStrLn "Result: "
    putStrLn (poliDerivative (parsePolinomial polinomial) (head symbol))
    loop