module Examples where

import Syntax


-- Multiply : Integer x Integer -> Integer
-- Multiply (m, n) =
--  if m == 0 then 0 else n + Multiply (m - 1, n)
-- property commutative m n . Multiply (m, n) == Multiply (n, m)

-- (we save space in the paper by desugaring:)
-- x == y ~> x <= y /\ y <= x
--        ~> if x <= y then y <= x else false

example1 :: Program Integer
example1 =
  Declaration "Multiply" ((Integer' :*: Integer') :->: Integer') $
  Definition  "Multiply"
    (Lambda "pair" (
      If (Leq (Fst (Variable "pair" 3) 4) (Number 0 5) 2)
         (Number 0 6)
         (Application (Variable "Multiply" 8)
            (Pair (Plus (Fst (Variable "pair" 12) 11) (Number (-1) 13) 10)
                  (Snd (Variable "pair" 15) 14) 9) 7) 1) 0) $
  Property "commutative" ["m", "n"]
   (If (Leq (Application (Variable "Multiply" 19) (Pair (Variable "m" 21) (Variable "n" 22) 20) 18)
            (Application (Variable "Multiply" 24) (Pair (Variable "n" 26) (Variable "m" 27) 25) 23) 17)
       (Leq (Application (Variable "Multiply" 30) (Pair (Variable "n" 32) (Variable "m" 33) 31) 29)
            (Application (Variable "Multiply" 35) (Pair (Variable "m" 37) (Variable "n" 38) 36) 34) 28)
       (Boolean False 39) 16) $
  EndOfProgram
