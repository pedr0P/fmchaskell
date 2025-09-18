module FMCBabyNat where

-- Do not alter this import!
import Prelude ( Show(..) , Eq(..) , undefined, IO(..), putStrLn, getLine)

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

-- define a new data type called Nat by listing all forms
data Nat = O | S Nat
  deriving (Eq, Show)

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
eleven = ten
twelve = eleven
thirteen = twelve
fourteen = thirteen
fifteen = fourteen
sixteen = fifteen
seventeen = sixteen
eighteen = seventeen
nineteen = eighteen
twenty = nineteen

-- addition
(+) :: Nat -> Nat -> Nat
n + O   = n
n + S m = S (n + m)

 -- syntactic associativity: L
 -- syntactic precedence: 6
infixl 6 +

-- Output: O means False, S O means True
isZero :: Nat -> Nat
isZero O = one
isZero _ = zero

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = zero
pred (S n) = n


-- Output: O means False, S O means True
even :: Nat -> Nat
even O = one
even (S O) = zero
even (S (S n)) = even n

odd :: Nat -> Nat
odd O = zero
odd (S O) = one
odd (S (S n)) = odd n

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
(*) :: Nat -> Nat -> Nat
(*) _ O = O
(*) n (S m) = n + (n * m)

infixl 7 *

-- exponentiation
(^) :: Nat -> Nat -> Nat
(^) _ O = one
(^) n (S m) = n * (n ^ m)

-- decision:
infixr 8 ^

-- PEDRO'S
-- greater than or equal to
(>=) :: Nat -> Nat -> Nat
(>=) O O = S O
(>=) _ O = S O
(>=) O _ = O
(>=) (S n) (S m) = (>=) n m

-- quotient
(/) :: Nat -> Nat -> Nat
(/) O _ = O
(/) n m = case (>=) n m of
            O -> O
            S O -> S ((/) (monus n m) m)

-- remainder
(%) :: Nat -> Nat -> Nat
(%) n m = monus n (m * ((/) n m))

-- divides
-- just for a change, we start by defining the "symbolic" operator
-- and then define `devides` as a synonym to it
-- again, outputs: O means False, S O means True
(|||) :: Nat -> Nat -> Nat
(|||) O _ = O
(|||) n m = isZero ((%) m n )

-- x `absDiff` y = |x - y|
-- (Careful here: this - is the actual minus operator we know from the integers!)
absDiff :: Nat -> Nat -> Nat
absDiff n O = n
absDiff O n = n
absDiff (S n) (S m) = absDiff n m

(|-|) :: Nat -> Nat -> Nat
(|-|) = absDiff

factorial :: Nat -> Nat
factorial O = one
factorial (S n) = (S n) * (factorial n)

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = zero
sg _ = one

-- lg :: Nat -> Nat -> Nat
-- lg = S lo

-- PEDRO's
-- not
not :: Nat -> Nat
not O = one
not _ = zero

-- PEDRO's
-- less than or equal to
(<=) :: Nat -> Nat -> Nat
(<=) O _ = one
(<=) _ O = zero
(<=) (S n) (S m) = (<=) n m
-- ,q:
-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo O _ = O
lo _ O = O
lo n m = case ((<=) n m) of
            O -> O
            S O -> S (lo n (((/) m n) + (%) m n))
