data Monomial = Monomial Coefficient Symbol Exponent deriving (Eq, Show)

type Coefficient = Int
type Symbol = [Char]
type Exponent = Int

-- Getters

getCoefficient :: Monomial -> Int
getCoefficient (Monomial coef symb exp) = coef

getSymbol :: Monomial -> [Char]
getSymbol (Monomial coef symb exp) = symb

getExponent :: Monomial -> Int
getExponent (Monomial coef symb exp) = exp


-- Useful functions

sortPol :: [Monomial] -> [Monomial]
sortPol [] = []
sortPol ((Monomial coef1 symb1 exp1) : polinomial) = sortPol first ++ [Monomial coef1 symb1 exp1] ++ sortPol last
    where first = [monomial | monomial <- polinomial, getSymbol monomial < symb1 || (getSymbol monomial == symb1 && getExponent monomial >= exp1)]
          last = [monomial | monomial <- polinomial, getSymbol monomial > symb1 || (getSymbol monomial == symb1 && getExponent monomial < exp1)]


removeZeros :: [Monomial] -> [Monomial]
removeZeros polinomial = [monomial | monomial <- polinomial, getCoefficient monomial /= 0]

canSum :: Monomial -> Monomial -> Bool
canSum monomial1 monomial2 = getSymbol monomial1 == getSymbol monomial2 && getExponent monomial1 == getExponent monomial2


sum2Monomials :: Monomial -> Monomial -> Monomial
sum2Monomials monomial1 monomial2 | canSum monomial1 monomial2
                                        = Monomial (getCoefficient monomial1 + getCoefficient monomial2) (getSymbol monomial1) (getExponent monomial1)
                                  | otherwise = error "Can't sum these two monomials"

normalizeAux :: [Monomial] -> [Monomial] -> [Monomial]
normalizeAux [] acc = acc
normalizeAux [onePol] [] = [onePol]
normalizeAux [onePol] acc = acc ++ [onePol] 
normalizeAux (firstPol : secondPol : polinomial) acc | canSum firstPol secondPol = normalizeAux polinomial (acc ++ [sum2Monomials firstPol secondPol])
                                                     | otherwise = normalizeAux (secondPol : polinomial) (acc ++ [firstPol])     

-- normalize Polinomial - a)

normalize :: [Monomial] -> [Monomial]
normalize polinomial = normalizeAux polinomialClean []
    where polinomialClean = sortPol $ removeZeros polinomial

-- Polinomial to test

polinomialTest :: [Monomial]
polinomialTest = [Monomial 0 "x" 2, Monomial 2 "y" 1, Monomial 5 "z" 1, Monomial 1 "y" 1, Monomial 7 "y" 2]
