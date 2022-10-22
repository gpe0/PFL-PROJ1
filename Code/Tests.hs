{-# LANGUAGE TemplateHaskell #-}

import Polinomial
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
prop_sumCommutative p1 p2 = sumPolinomial p1 p2 == sumPolinomial p2 p1

prop_sumAssociative :: [Monomial] -> [Monomial] -> [Monomial] -> Bool
prop_sumAssociative p1 p2 p3 = sumPolinomial (parsePolinomial (sumPolinomial p1 p2)) p3 == sumPolinomial p1 (parsePolinomial (sumPolinomial p2 p3))

prop_AdditiveIdentity :: [Monomial] -> Bool
prop_AdditiveIdentity p1 = sumPolinomial p1 [Monomial 0 []] == normalize p1

prop_AdditiveInverse :: [Monomial] -> Bool
prop_AdditiveInverse p1 = sumPolinomial p1 (parsePolinomial (multiplyPolinomial p1 [Monomial (-1) []])) == "0"

-- Testing multiplication c)

prop_multiplicationCommutative :: [Monomial] -> [Monomial] -> Bool
prop_multiplicationCommutative p1 p2 = isListIn ex1 ex2 && isListIn ex2 ex1
            where   ex1 = parsePolinomial (multiplyPolinomial p1 p2)
                    ex2 = parsePolinomial (multiplyPolinomial p2 p1)

prop_multiplicationDistributive :: [Monomial] -> [Monomial] -> [Monomial] -> Bool
prop_multiplicationDistributive p1 p2 p3 = isListIn ex1 ex2 && isListIn ex2 ex1
        where   ex1 = parsePolinomial (multiplyPolinomial p1 (parsePolinomial (sumPolinomial p2 p3)))
                ex2 = parsePolinomial (sumPolinomial (parsePolinomial (multiplyPolinomial p1 p2)) (parsePolinomial (multiplyPolinomial p1 p3)))


return []

runTests :: IO Bool
runTests = $quickCheckAll