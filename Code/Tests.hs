{-# LANGUAGE TemplateHaskell #-}

import Polynomial
import Test.QuickCheck


isValIn :: Eq a => [a] -> a -> Bool
isValIn [] x = False
isValIn (element:rest) x = (element == x) || isValIn rest x

bigAnd :: [Bool] -> Bool
bigAnd [oneAnd] = oneAnd
bigAnd (x:xs) = x && bigAnd xs

isListIn :: Eq a => [a] -> [a] -> Bool
isListIn [] [] = True
isListIn [] l2 = False
isListIn l1 l2 = bigAnd [isValIn l1 x2 | x2 <- l2]

instance Arbitrary Monomial where
    arbitrary = do
        coef <- arbitrary
        symb <- choose('a', 'z')
        exp <- suchThat arbitrary (> 0)
        return $ Monomial coef [Symbol symb exp]


-- Testing normalize a)
prop_normalizeExtraZeroValue :: [Monomial] -> Bool
prop_normalizeExtraZeroValue p = normalize p == normalize (Monomial 0 []:p)

prop_normalizeReverse :: [Monomial] -> Bool
prop_normalizeReverse p = normalize p == normalize (reverse p)


-- Testing sum b)
prop_sumCommutative :: [Monomial] -> [Monomial] -> Bool
prop_sumCommutative p1 p2 = sumPolynomial p1 p2 == sumPolynomial p2 p1

prop_sumAssociative :: [Monomial] -> [Monomial] -> [Monomial] -> Bool
prop_sumAssociative p1 p2 p3 = sumPolynomial (sumPolynomial p1 p2) p3 == sumPolynomial p1 (sumPolynomial p2 p3)

prop_additiveIdentity :: [Monomial] -> Bool
prop_additiveIdentity p1 = sumPolynomial p1 [Monomial 0 []] == normalize p1

prop_additiveInverse :: [Monomial] -> Bool
prop_additiveInverse p1 = null (sumPolynomial p1 (multiplyPolynomial p1 [Monomial (-1) []]))

-- Testing multiplication c)

prop_multiplicationCommutative :: [Monomial] -> [Monomial] -> Bool
prop_multiplicationCommutative p1 p2 = isListIn ex1 ex2 && isListIn ex2 ex1
            where   ex1 = multiplyPolynomial p1 p2
                    ex2 = multiplyPolynomial p2 p1

prop_multiplicationDistributive :: [Monomial] -> [Monomial] -> [Monomial] -> Bool
prop_multiplicationDistributive p1 p2 p3 = isListIn ex1 ex2 && isListIn ex2 ex1
        where   ex1 = multiplyPolynomial p1 (sumPolynomial p2 p3)
                ex2 = sumPolynomial (multiplyPolynomial p1 p2) (multiplyPolynomial p1 p3)

prop_multiplicationIdentity :: [Monomial] -> Bool
prop_multiplicationIdentity p1 = multiplyPolynomial p1 [Monomial 1 []] == normalize p1

prop_multiplicationAbsorving :: [Monomial] -> Bool
prop_multiplicationAbsorving p1 = null (multiplyPolynomial p1 [Monomial 0 []])

-- Testing derivative d)

prop_derivativeSum :: [Monomial] -> [Monomial] -> Char -> Bool
prop_derivativeSum p1 p2 x = ex1 == ex2
        where   ex1 = poliDerivative (sumPolynomial p1 p2) x
                ex2 = sumPolynomial (poliDerivative p1 x) (poliDerivative p2 x)

return []

runTests :: IO Bool
runTests = $quickCheckAll