import Polynomial
import Control.Monad (when)

main :: IO()
main = 
    do
        putStr "PFL Project 1 - Polynomials\n"          
        loop

        putStr "\nFinishing Program\n"
        return()



loop :: IO()
loop = 
    do
        putStr "\nWhat do you want to do:\n"
        putStr "1 - Normalize polynomial\n"
        putStr "2 - Sum two polynomials\n"
        putStr "3 - Multiply two polynomials\n"
        putStr "4 - derive a polynomial\n"
        putStr "Anything else - Quit\n"
        putStr "Choice: "
        choice <- getLine
        when (choice == "1") loop1
        when (choice == "2") loop2
        when (choice == "3") loop3
        when (choice == "4") loop4

loop1 :: IO()
loop1 = do
    putStr "Write a polynomial: "
    polynomial <- getLine
    putStrLn "Result: "
    putStrLn (polynomialToString (normalize (parsePolynomial polynomial)))
    loop

loop2 :: IO()
loop2 = do
    putStr "Write a polynomial: "
    polynomial1 <- getLine
    putStr "Write another polynomial: "
    polynomial2 <- getLine
    putStrLn "Result: "
    putStrLn (polynomialToString (sumPolynomial (parsePolynomial polynomial1) (parsePolynomial polynomial2)))
    loop

loop3 :: IO()
loop3 = do
    putStr "Write a polynomial: "
    polynomial1 <- getLine
    putStr "Write another polynomial: "
    polynomial2 <- getLine
    putStrLn "Result: "
    putStrLn (polynomialToString (multiplyPolynomial (parsePolynomial polynomial1) (parsePolynomial polynomial2)))
    loop

loop4 :: IO()
loop4 = do
    putStr "Write a polynomial: "
    polynomial <- getLine
    putStr "Write the symbol to be derived to (one char): "
    symbol <- getLine
    putStrLn "Result: "
    putStrLn (polynomialToString (poliDerivative (parsePolynomial polynomial) (head symbol)))
    loop