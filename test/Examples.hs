module Examples where

import Syntax

-- Multiply : Integer x Integer -> Integer
-- Multiply (m, n) =
--  if m == 0 then 0 else n + Multiply (m - 1, n)

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
  EndOfProgram
