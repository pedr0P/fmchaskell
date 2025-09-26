module ExBool where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Enum(..)
    , Integral(..)
    , Int
    , Char
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Bool = False | True

-- Sugars:
tt, ff :: Bool
tt = True
ff = False

instance Show Bool where
    show True = "True"
    show False = "False" 

instance Enum Bool where
    fromEnum True  = 1
    fromEnum False = 0
    toEnum 1 = True
    toEnum 0 = False

-- conjunction (AND)
(&&) :: Bool -> Bool -> Bool
(&&) True True = True
(&&) _ _ = False

infixr 3 &&

-- disjunction (OR)
(||) :: Bool -> Bool -> Bool
(||) False False  = False
(||) _ _ = True


infixr 2 ||

-- NAND (Sheffer stroke)
(/|\) :: Bool -> Bool -> Bool
(/|\) b = not . ((&&) b)

infixr 2 /|\

-- NOR (aka: Peirce arrow or Quine dagger)
(\|/) :: Bool -> Bool -> Bool
(\|/) = (&&)

infixr 2 \|/

-- XOR (exclusive disjunction)
(<=/=>) :: Bool -> Bool -> Bool
(<=/=>) False False = False
(<=/=>) True True = False
(<=/=>) _ _ = True

infixr 2 <=/=>

-- boolean negation
not :: Bool -> Bool
not True = False
not _ = True

-- if-then-else expression
ifThenElse :: Bool -> a -> a -> a
ifThenElse b x y =
    case b of
        True -> x
        False -> y

-- logical "implies"
(==>) :: Bool -> Bool -> Bool
(==>) True False = False
(==>) _ _ = True

infixr 1 ==>

-- logical "implied by"
(<==) :: Bool -> Bool -> Bool
(<==) False True = False
(<==) _ _ = True

infixl 1 <==

-- logical equivalence
(<=>) :: Bool -> Bool -> Bool
(<=>) b l = (b <== l) && (b ==> l)

infixr 1 <=>


