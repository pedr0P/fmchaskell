{-# LANGUAGE GADTs #-}

module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral(..)
    , Bool(..) , not , (&&) , (||)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    , (++)
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat where
  O :: Nat
  S :: Nat -> Nat

----------------------------------------------------------------
-- typeclass implementations
----------------------------------------------------------------

-- some sugar
zero, one, two, three, four, five, six, seven, eight :: Nat
zero  = O
one   = S zero
two   = S one
three = S two
four  = S three
five  = S four
six   = S five
seven = S six
eight = S seven
nine  = S eight
ten   = S nine
eleven = S ten
twelve = S eleven
thirteen = S twelve
fourteen = S thirteen
fifteen = S fourteen
sixteen = S fifteen
seventeen = S sixteen
eighteen = S seventeen
nineteen = S eighteen
twenty = S nineteen
instance Show Nat where
    -- zero  should be shown as O
    -- three should be shown as SSSO
    show O = "O"
    show (S n) = "S" ++ show n

instance Eq Nat where
    (==) O O = True
    (==) (S n) (S m) = (==) n m
    (==) _ _ = False

instance Ord Nat where
    (<=) O _ = True
    (<=) _ O = False
    (<=) (S n) (S m) = (<=) n m

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min (S n) (S m) = S (min n m)
    min _ _ = O
    max (S n) (S m) = S (min n m)
    max n O = n
    max O m = m


----------------------------------------------------------------
-- internalized predicates
----------------------------------------------------------------

isZero :: Nat -> Bool
isZero O = True
isZero _ = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = O
pred (S n) = n

even :: Nat -> Bool
even O = True
even (S O) = False
even (S (S n)) = even n

odd :: Nat -> Bool
odd O = False
odd (S O) = True
odd (S (S n)) = odd n

----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
n <+> O = n
n <+> (S m) = S (n <+> m)

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus n O = n
monus O _ = O
monus (S n) (S m) = monus n m

(-*) :: Nat -> Nat -> Nat
(-*) = monus

-- multiplication
times :: Nat -> Nat -> Nat
times _ O = O
times n (S m) = n + (n * m)

(<*>) :: Nat -> Nat -> Nat
(<*>) = times

-- power / exponentiation
pow :: Nat -> Nat -> Nat
pow _ O = one
pow n (S m) = n * (pow n m)

exp :: Nat -> Nat -> Nat
exp = pow

(<^>) :: Nat -> Nat -> Nat
(<^>) = exp

-- quotient
(</>) :: Nat -> Nat -> Nat
O </> _ = O
n </> m = if (n >= m) then S ((n -* m) </> m) else O

-- remainder
(<%>) :: Nat -> Nat -> Nat
n <%> m = monus n (m * (n </> m))

-- euclidean division
-- eucdiv :: (Nat, Nat) -> (Nat, Nat)
-- eucdiv (n, m)= ()

-- divides
(<|>) :: Nat -> Nat -> Bool
(<|>) n m = if (n <%> m == O) then True else False

divides = (<|>)

-- distance between nats
-- x `dist` y = |x - y|
-- (Careful here: this - is the real minus operator!)
dist :: Nat -> Nat -> Nat
dist n O = n
dist O n = n
dist (S n) (S m) = dist n m

(|-|) = dist

factorial :: Nat -> Nat
factorial O = one
factorial (S n) = (S n) * (factorial n)

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = zero
sg _ = one

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo O _ = error "Logarithm at base 0"
lo _ O = error "Logarithm of 0"
lo (S O) _ = error "Logarithm at base 1"
lo n m
  | (m </> n) == S O = O
  | otherwise = one + lo n (m </> n)


----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: Integral a => a -> Nat
toNat 0 = O
toNat i = S (toNat (i-1))

fromNat :: Integral a => Nat -> a
fromNat O = 0
fromNat (S n) = 1 + (fromNat n)


-- Voilá: we can now easily make Nat an instance of Num.
instance Num Nat where
    (+) = (<+>)
    (*) = (<*>)
    (-) = (-*)
    abs n = n
    signum = sg
    fromInteger x
      | x < 0     = error "Negative integer does not have Nat counterpart"
      | x == 0    = toNat x
      | otherwise = toNat x

