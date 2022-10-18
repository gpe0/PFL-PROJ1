import Polinomial
import Test.QuickCheck

instance Arbitrary Monomial where
    arbitrary = do
        coef <- arbitrary
        symb <- choose('a', 'z')
        exp <- arbitrary
        return $ Monomial coef [Symbol symb exp]

propNormalize :: [Monomial] -> Bool
propNormalize p = normalize p == normalize (Monomial 0 []:p)