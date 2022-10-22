module Polinomial where

import Data.Char ( isNumber, isAlpha )
data Symbol = Symbol Char Exponent deriving (Eq, Show)

data Monomial = Monomial Coefficient Symbols deriving (Eq, Show)

type Symbols = [Symbol]
type Coefficient = Int
type Exponent = Int

-- Getters

-- Get char part of the symbol
getSymbol :: Symbol -> Char
getSymbol (Symbol symb exp) = symb

-- Get exponent of the symbol
getExponent :: Symbol -> Int
getExponent (Symbol symb exp) = exp

-- Get coefficient of a monomial
getCoefficient :: Monomial -> Int
getCoefficient (Monomial coef symbs) = coef

-- Get list of symbols (char part) of a monomial
getSymbols :: Monomial -> [Char]
getSymbols (Monomial coef symbs) = [getSymbol symb | symb <- symbs]

-- Get the sum of the exponents of a monomial
getExponents :: Monomial -> Int
getExponents (Monomial coef symbs) = sum [getExponent symb | symb <- symbs] -- sort

-- Get list of exponents (of every symbol) of a monomial
getExponentsList :: Monomial -> [Int]
getExponentsList (Monomial coef symbs) = [getExponent symb | symb <- symbs]

-- Given a symbol and a monomial, get the respective exponent
getExponentFromSymbol :: Monomial -> Char -> Int
getExponentFromSymbol (Monomial coef symbs) symb = head [getExponent x | x <- symbs, getSymbol x == symb]

-- Get the Symbol list of a monomial
getSymbolVariables :: Monomial -> Symbols
getSymbolVariables (Monomial coef symbs) = symbs


-- Useful functions

-- Check whether a monomial as a certain symbol or not
hasSymbol :: Monomial -> Char -> Bool
hasSymbol monomial symbol = [x | x <- getSymbols monomial, x == symbol] /= []

-- Sorts the polinomial (bigger exponent -> smaller symbol ('x' < 'y'))
sortPol :: [Monomial] -> [Monomial]
sortPol [] = []
sortPol ((Monomial coef1 symb1) : polinomial) = sortPol first ++ [Monomial coef1 symb1] ++ sortPol last
    where first = [monomial | monomial <- polinomial, getExponents monomial > getExponents mono || (getExponents monomial == getExponents mono && getSymbols monomial < getSymbols mono)
                    ]
          last = [monomial | monomial <- polinomial, getExponents monomial < getExponents mono || (getExponents monomial == getExponents mono && getSymbols monomial >= getSymbols mono)
                    ]
          mono = Monomial coef1 symb1

-- Sorts symbols (smaller char ('x' < 'y') -> bigger exponent)
sortSymbols :: Symbols -> Symbols
sortSymbols [] = []
sortSymbols ((Symbol symb1 exp1) : symbols) = sortSymbols first ++ [Symbol symb1 exp1] ++ sortSymbols last
    where first = [s | s <- symbols, getSymbol s < getSymbol symbol || ( getSymbol s == getSymbol symbol && getExponent s > getExponent symbol)]
          last = [s | s <- symbols, getSymbol s > getSymbol symbol || ( getSymbol s == getSymbol symbol && getExponent s <= getExponent symbol)]
          symbol = Symbol symb1 exp1     


-- Remove monomials with a zero coefficient
removeZeros :: [Monomial] -> [Monomial]
removeZeros polinomial = [monomial | monomial <- polinomial, getCoefficient monomial /= 0]

-- Check whether two monomials can sum or not
canSum :: Monomial -> Monomial -> Bool
canSum monomial1 monomial2 = getSymbols monomial1 == getSymbols monomial2 && [getExponentsList monomial1] == [getExponentsList monomial2]

-- Changes the last value of an array by a given one
substituteLastVal :: [a] -> a -> [a]
substituteLastVal l x = reverse (x : rest)
    where (last:rest) = reverse l

-- Tries to sum 2 monomials
sum2Monomials :: Monomial -> Monomial -> Monomial
sum2Monomials monomial1 monomial2 | canSum monomial1 monomial2
                                        = Monomial (getCoefficient monomial1 + getCoefficient monomial2) [Symbol symb exp | (symb, exp) <- zip (getSymbols monomial1) (getExponentsList monomial1)]
                                  | otherwise = error "Can't sum these two monomials"

-- aux normalize function to symbols
normalizeSymbAux :: Symbols -> Symbols -> Symbols
normalizeSymbAux [] acc = acc
normalizeSymbAux [oneSymb] [] = [oneSymb]
normalizeSymbAux (firstSymb : symbols) [] = normalizeSymbAux symbols [firstSymb]
normalizeSymbAux (firstSymb : symbols) acc | getSymbol lastAccumulated == getSymbol firstSymb = 
                                                    normalizeSymbAux symbols (
                                                        substituteLastVal acc (
                                                            Symbol (getSymbol lastAccumulated) (getExponent lastAccumulated + getExponent firstSymb)
                                                        )
                                                    )   
                                           | otherwise = normalizeSymbAux symbols (acc ++ [firstSymb]) 
        where lastAccumulated = last acc

-- normalizes a list of symbols
normalizeSymbols :: Symbols -> Symbols
normalizeSymbols symbols = normalizeSymbAux symbolsOrdered []
    where symbolsOrdered = sortSymbols symbols

-- Sum a list of monomials (must check before if you can sum them)
sumMultipleMonomials :: [Monomial] -> Monomial -> Monomial
sumMultipleMonomials [] acc = acc
sumMultipleMonomials (one:rest) acc = sumMultipleMonomials rest (sum2Monomials one acc) 

-- aux normalize function to polinomials ("bullet-proof version")
normalizePoliAux :: [Monomial] -> [Monomial] -> [Monomial]
normalizePoliAux [] acc = acc
normalizePoliAux [onePol] [] = [onePol]
normalizePoliAux (firstPol : polinomial) [] = normalizePoliAux polinomial [firstPol]
normalizePoliAux polinomial acc | null filteredList = normalizePoliAux rest (acc ++ [first])
                                | otherwise =  normalizePoliAux (restList ++ otherSymbols) (substituteLastVal acc (sumMultipleMonomials (lastAccumulated : filteredList) (Monomial 0 (getSymbolVariables lastAccumulated))))
        where   lastAccumulated = last acc
                sameSymbols = takeWhile (\mono -> getExponents lastAccumulated == getExponents mono) polinomial
                otherSymbols = dropWhile(\mono -> getExponents lastAccumulated == getExponents mono) polinomial
                filteredList = filter (canSum lastAccumulated) sameSymbols
                restList = filter (\mono -> getSymbolVariables lastAccumulated /= getSymbolVariables mono) sameSymbols
                (first:rest) = restList ++ otherSymbols

-- aux normalize function to polinomials ("faster version")
normalizePoliAux' :: [Monomial] -> [Monomial] -> [Monomial]
normalizePoliAux' [] acc = acc
normalizePoliAux' [onePol] [] = [onePol]
normalizePoliAux' (firstPol : polinomial) [] = normalizePoliAux polinomial [firstPol]
normalizePoliAux' (firstPol : polinomial) acc | canSum firstPol lastAccumulated = normalizePoliAux polinomial (substituteLastVal acc (sum2Monomials firstPol lastAccumulated))
                                         | otherwise = normalizePoliAux polinomial (acc ++ [firstPol])  
        where lastAccumulated = last acc

-- multiplies 2 monomials
multiplyMonomial :: Monomial -> Monomial -> Monomial
multiplyMonomial monomial1 monomial2 = Monomial (getCoefficient monomial1 * getCoefficient monomial2) (([Symbol symb exp | (symb, exp) <- zip (getSymbols monomial1) (getExponentsList monomial1)])++([Symbol symb exp | (symb, exp) <- zip (getSymbols monomial2) (getExponentsList monomial2)]))

-- multiplies a monomial with a polinomial
multiplyMonomialAndPolinomial :: Monomial -> [Monomial] -> [Monomial]
multiplyMonomialAndPolinomial monomial = map (multiplyMonomial monomial)

-- derivates a monomial, to a given symbol
derivative :: Monomial -> Char -> Monomial
derivative monomial symbol | hasSymbol monomial symbol && getExponentFromSymbol monomial symbol > 1 = Monomial (getExponentFromSymbol monomial symbol * getCoefficient monomial)  [if symb == symbol then Symbol symb (exp - 1) else Symbol symb exp | (symb, exp) <- zip (getSymbols monomial) (getExponentsList monomial)]
                           | hasSymbol monomial symbol && getExponentFromSymbol monomial symbol == 1 = Monomial (getExponentFromSymbol monomial symbol * getCoefficient monomial)  [Symbol symb exp | (symb, exp) <- zip (getSymbols monomial) (getExponentsList monomial), symb /= symbol]
                           | otherwise = Monomial 0 []                                    


-- normalize Polinomial - a)

normalize :: [Monomial] -> [Char]
normalize polinomial = polinomialToString $ removeZeros $ normalizePoliAux polinomialSorted []
    where polinomialSorted = sortPol polinomialSymbolsSorted
          polinomialSymbolsSorted = [Monomial coef (normalizeSymbols symbols) | Monomial coef symbols <- polinomial]

-- Adition -b)

sumPolinomial :: [Monomial] -> [Monomial] -> [Char]
sumPolinomial polinomial1 polinomial2 = normalize (polinomial1 ++ polinomial2)

-- Product -c)

multiplyPolinomial :: [Monomial] -> [Monomial] -> [Char]
multiplyPolinomial polinomial1 polinomial2 = normalize (concat [ multiplyMonomialAndPolinomial monomial2 polinomial1 | monomial2 <- polinomial2])

-- Derivative - d)
poliDerivative :: [Monomial] -> Char -> [Char]
poliDerivative polinomial symbol = normalize [derivative monomial symbol | monomial <- polinomial]



-- Polinomial to test

polinomialTestSmall :: [Monomial]
polinomialTestSmall = [Monomial 2 [Symbol 'y' 1]]

polinomialTest :: [Monomial]
polinomialTest = [Monomial 0 [Symbol 'x' 2], Monomial 2 [Symbol 'y' 1], Monomial 5 [Symbol 'z' 1], Monomial 1 [Symbol 'y' 1], Monomial 7 [Symbol 'y' 2]]

poliInStringTest :: [Char]
poliInStringTest = "0*x^2 + 2y + 5*z^1 - 7*y^2 + x -54*x*y^2*z^3"

-- Parsing Functions

-- Removes spaces " " and multiplication symbol "*" from a string
removeSpacesAndMult :: [Char] -> [Char]
removeSpacesAndMult  text = [c | c <- text, c /= ' ', c/= '*']

-- Removes characters until it reaches one of the end chars
removeCharsTillOrTill :: [Char] -> Char -> Char -> [Char]
removeCharsTillOrTill [] _ _ = []
removeCharsTillOrTill (x:xs) char1 char2 = if x == char1 || x == char2 then x:xs else removeCharsTillOrTill xs char1 char2

-- Get chars that pass a given expression
onlyGetCharsThat :: [Char] -> (Char -> Bool) -> [Char]
onlyGetCharsThat [] _ = []
onlyGetCharsThat (x:xs) f = if f x then x : onlyGetCharsThat xs f else onlyGetCharsThat xs f

-- normalizes the exponent values
normalizeExp :: [Char] -> [Char]
normalizeExp [] = []
normalizeExp [x1] = if isAlpha x1 then [x1, '^', '1'] else [x1]
normalizeExp [x1, x2] = if isAlpha x2 then [x1, x2, '^', '1'] else [x1, x2]
normalizeExp (x1:x2:xs) = if isAlpha x1 && x2 /= '^' then x1 : '^' : '1' : x2 : normalizeExp xs else x1 : x2 : normalizeExp xs

-- extract the next monomial coefficient from a given string
extractCoef :: [Char] -> [Char]
extractCoef [] = []
extractCoef (x:xs)  | x == '+' = extractCoef xs
                    | x == '-' = '-' : extractCoef xs
                    | otherwise = if null (takeWhile isNumber (x:xs)) then "1" else takeWhile isNumber (x:xs)

-- extract the next monomial symbols from a given string
extractSymb :: [Char] -> [Char]
extractSymb [] = []
extractSymb (x:xs)  | x == '+' || x == '-' = extractSymb xs
                    | otherwise = onlyGetCharsThat (takeWhile (\x -> x /= '+' && x /= '-') (dropWhile isNumber (x:xs))) isAlpha

-- creates an exponent list (intermidiate function)
createExpList :: [Char] -> [Int] -> [Int]
createExpList [] acc = acc
createExpList expression acc = createExpList nextIter (acc ++ [read exponent :: Int])
    where   nextExpression = dropWhile ( /= '^' ) expression
            exponent = takeWhile isNumber (drop 1 nextExpression)
            nextIter = dropWhile ( /= '^' ) (drop 1 nextExpression)

-- extract the next monomial exponents from a given string
extractExp :: [Char] -> [Int]
extractExp [] = []
extractExp (x:xs)   | x == '+' || x == '-' = extractExp xs
                    | otherwise = createExpList symbolsAndExp []
        where   symbolsAndExp = takeWhile (\x -> x /= '+' && x /= '-') (dropWhile isNumber (x:xs))

-- aux parse function
parseAux :: [Char] -> [Monomial] -> [Monomial]
parseAux [] acc = acc
parseAux poliText acc = parseAux (removeCharsTillOrTill chars '+' '-') (acc ++ [newMonomial])
    where   newMonomial = Monomial (read (extractCoef poliText) :: Int) [Symbol symb exp | (symb, exp) <- zip (extractSymb poliText) (extractExp poliText)]
            (firstChar:chars) = poliText

-- Parses a string polinomial into the Polinomial representation
parsePolinomial :: [Char] -> [Monomial]
parsePolinomial poliText = parseAux (normalizeExp $ removeSpacesAndMult poliText) []

-- Remove outter brackets from a given array
removeOutterBrackets :: [[a]] -> [a]
removeOutterBrackets [] = []
removeOutterBrackets [[]] = []
removeOutterBrackets (x:xs) = [item | item <- x] ++ removeOutterBrackets xs

-- Transforms a monomial into a string
monomialToString :: Monomial -> [Char]
monomialToString (Monomial coef symbols)    | coef < 0 = show coef ++ removeOutterBrackets [[symb] ++ "^" ++ show exp | Symbol symb exp <- symbols]
                                            | otherwise = "+" ++ show coef ++ removeOutterBrackets [[symb] ++ "^" ++ show exp | Symbol symb exp <- symbols]

-- Transforms a polinomial into a string
polinomialToString :: [Monomial] -> [Char]
polinomialToString [] = "0"
polinomialToString polinomial = if signChar == '+' then stringPoli else signChar:stringPoli
        where (signChar:stringPoli) = removeOutterBrackets [monomialToString monomial | monomial <- polinomial]