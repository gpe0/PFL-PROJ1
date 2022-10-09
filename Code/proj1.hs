data Monomial = Monomial Coefficient Symbol Exponent deriving (Eq, Show)

type Coefficient = Int
type Symbol = Char
type Exponent = Int

normalize :: [Monomial] -> [Monomial]
normalize polinomial = polinomial