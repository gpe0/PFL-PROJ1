data Symbol = Symbol Char Exponent deriving (Eq, Show)

data Monomial = Monomial Coefficient Symbols deriving (Eq, Show)

type Symbols = [Symbol]
type Coefficient = Int
type Exponent = Int

-- Getters

getSymbol :: Symbol -> Char
getSymbol (Symbol symb exp) = symb

getExponent :: Symbol -> Int
getExponent (Symbol symb exp) = exp

getCoefficient :: Monomial -> Int
getCoefficient (Monomial coef symbs) = coef

getSymbols :: Monomial -> [Char]
getSymbols (Monomial coef symbs) = [getSymbol symb | symb <- symbs]

getExponents :: Monomial -> Int
getExponents (Monomial coef symbs) = sum [getExponent symb | symb <- symbs]

getExponentsList :: Monomial -> [Int]
getExponentsList (Monomial coef symbs) = [getExponent symb | symb <- symbs]

getExponentFromSymbol :: Monomial -> Char -> Int
getExponentFromSymbol (Monomial coef symbs) symb = head [getExponent x | x <- symbs, getSymbol x == symb]


-- Useful functions

hasSymbol :: Monomial -> Char -> Bool
hasSymbol monomial symbol = [x | x <- getSymbols monomial, x == symbol] /= []

sortPol :: [Monomial] -> [Monomial]
sortPol [] = []
sortPol ((Monomial coef1 symb1) : polinomial) = sortPol first ++ [Monomial coef1 symb1] ++ sortPol last
    where first = [monomial | monomial <- polinomial, getSymbols monomial < getSymbols mono || (getSymbols monomial == getSymbols mono && getExponents monomial >= getExponents mono)]
          last = [monomial | monomial <- polinomial, getSymbols monomial > getSymbols mono || (getSymbols monomial == getSymbols mono && getExponents monomial < getExponents mono)]
          mono = Monomial coef1 symb1


removeZeros :: [Monomial] -> [Monomial]
removeZeros polinomial = [monomial | monomial <- polinomial, getCoefficient monomial /= 0]

canSum :: Monomial -> Monomial -> Bool
canSum monomial1 monomial2 = getSymbols monomial1 == getSymbols monomial2 && [getExponentsList monomial1] == [getExponentsList monomial2]


sum2Monomials :: Monomial -> Monomial -> Monomial
sum2Monomials monomial1 monomial2 | canSum monomial1 monomial2
                                        = Monomial (getCoefficient monomial1 + getCoefficient monomial2) [Symbol symb exp | (symb, exp) <- zip (getSymbols monomial1) (getExponentsList monomial1)]
                                  | otherwise = error "Can't sum these two monomials"

normalizeAux :: [Monomial] -> [Monomial] -> [Monomial]
normalizeAux [] acc = acc
normalizeAux [onePol] [] = [onePol]
normalizeAux [onePol] acc = acc ++ [onePol] 
normalizeAux (firstPol : secondPol : polinomial) acc | canSum firstPol secondPol = normalizeAux polinomial (acc ++ [sum2Monomials firstPol secondPol])
                                                     | otherwise = normalizeAux (secondPol : polinomial) (acc ++ [firstPol])  


derivative :: Monomial -> Char -> Monomial
derivative monomial symbol | hasSymbol monomial symbol && getExponentFromSymbol monomial symbol > 1 = Monomial (getExponentFromSymbol monomial symbol * getCoefficient monomial)  [if symb == symbol then Symbol symb (exp - 1) else Symbol symb exp | (symb, exp) <- zip (getSymbols monomial) (getExponentsList monomial)]
                           | hasSymbol monomial symbol && getExponentFromSymbol monomial symbol == 1 = Monomial (getExponentFromSymbol monomial symbol * getCoefficient monomial)  [if symb == symbol then Symbol '|' 0 else Symbol symb exp | (symb, exp) <- zip (getSymbols monomial) (getExponentsList monomial)]
                           | otherwise = Monomial 0 [Symbol '|' 0]                                    

-- normalize Polinomial - a)

normalize :: [Monomial] -> [Monomial]
normalize polinomial = normalizeAux polinomialClean []
    where polinomialClean = sortPol $ removeZeros polinomial


-- Derivative - d)
poliDerivative :: [Monomial] -> Char -> [Monomial]
poliDerivative polinomial symbol = normalize [derivative monomial symbol | monomial <- polinomial]

-- Polinomial to test

polinomialTest :: [Monomial]
polinomialTest = [Monomial 0 [Symbol 'x' 2], Monomial 2 [Symbol 'y' 1], Monomial 5 [Symbol 'z' 1], Monomial 1 [Symbol 'y' 1], Monomial 7 [Symbol 'y' 2]]
