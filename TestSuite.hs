module TestSuite where

import Exam

{-******************************************
*   TestSuite.hs               05.01.25    *
*        Author: Sandra K. Johansen        *
*                                          *
*   Requirements:                          *
*   imported Exam to run functions         *
*******************************************-}

{-
 Here, parts of the code itself gets tested,
 using the smallest and simplest programs that I,
 the author, could think of.
-}

{- testing moving data, returns 10 if functional -}
runMov :: Int
runMov  = interp [Movc R0 10, Mov R0 Rv] 

{- testing arithmetic, returns 20 if functional -}
runAdd :: Int
runAdd  = interp [Movc R0 10, Movc R1 10, Add R0 R1 Rv]

runAddc :: Int
runAddc = interp [Addc R0 20, Mov R0 Rv]

runSub :: Int
runSub  = interp [Movc R0 30, Movc R1 10, Sub R0 R1 Rv]

runSubc :: Int
runSubc = interp [Addc R0 30, Subc R0 10, Mov R0 Rv]

runMul :: Int
runMul  = interp [Addc R0 2, Addc R1 10, Mul R0 R1 Rv]

runMulc :: Int
runMulc = interp [Addc R0 2, Mulc R0 10, Mov R0 Rv]

runDiv :: Int
runDiv  = interp [Addc R0 40, Addc R1 2, Div R0 R1 Rv]

runDivc :: Int 
runDivc = interp [Addc R0 40, Divc R0 2, Mov R0 Rv]


{- testing jumps, returns 1 if true, otherwise 2 -}
runJmp :: Int
runJmp  = interp [Addc R0 2, Addc R1 1, Jmp 4, 
                  Mul R0 R1 R0, Sub R0 R1 R0, Mov R0 Rv]

runJal :: Int
runJal  = interp [Movc R0 10, Jal 2, Mov Lnk Rv]

runJif1 :: Int
runJif1 = interp [Movc R0 1, Jif 3 R0, Addc R0 1, Mov R0 Rv]

-- exception; runJif2 loops forever upon failure
runJif2 :: Int
runJif2 = interp [Movc R0 0, Jif 0 R0, Movc Rv 1]

runJr :: Int
runJr   = interp [Movc R0 3, Jr R0, Movc Rv 1, Addc Rv 1]


{-
and a....
 _                             
| |__   __ _ _ __  _ __  _   _ 
| '_ \ / _` | '_ \| '_ \| | | |
| | | | (_| | |_) | |_) | |_| |
|_| |_|\__,_| .__/| .__/ \__, |
            |_|   |_|    |___/ 
                                               _ 
 _ __   _____      __  _   _  ___  __ _ _ __  | |
| '_ \ / _ \ \ /\ / / | | | |/ _ \/ _` | '__| | |
| | | |  __/\ V  V /  | |_| |  __/ (_| | |    |_|
|_| |_|\___| \_/\_/    \__, |\___|\__,_|_|    (_)
                       |___/                     

-}