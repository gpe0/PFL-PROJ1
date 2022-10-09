data Monomial = Monomial Coefficient Symbol Exponent deriving (Eq, Show)

type Coefficient = Int
type Symbol = Char
type Exponent = Int

normalize :: [Monomial] -> [Monomial]
normalize [] = []
normalize (firstPol:polinomial) = if head [coef | Monomial coef symb exp <- [firstPol]] == 0 then normalize polinomial 
    else head [
    Monomial (
        head [coef + sum [coef2 | monomial <- polinomial, Monomial coef2 symb2 exp2 <- [monomial], symb == symb2, exp == exp2]])
    symb exp | Monomial coef symb exp <- [firstPol]] 
    : normalize [monomial | monomial <- polinomial, Monomial coef1 symb1 exp1 <- [firstPol], Monomial coef2 symb2 exp2 <- [monomial],
    symb1 /= symb2 || exp1 /= exp2]