module Exam where

{-******************************************
*   Exam.hs                    05.01.25    *
*        Author: Sandra K. Johansen        *
*                                          *
*   Specifications:                        *
*   All registers and instructions must    *
*   be capitalized, i.e have the first     *
*   letter capitalized, fx:                *
*               mov = Mov                  *
*               r1  = R1                   *
*   Furthermore, each instruction which    *
*   places information in a register will  *
*   place it in the last listed register.  *
*   i.e. "Add R1 R2 R3" places the result  *
*   in R3, while "Addc R1 5" places 5 in   *
*   R1.                                    *
*                                          *
*   Information:                           *
*   The text file "test1.txt" holds the    *
*   program example given in the exam      *
*   description, altered to fit the        *
*   requirements.                          *
*******************************************-}

--____________--
-- GROUNDWORK --
--____________--

-- RUN PROGRAM --
runProgram :: FilePath -> IO ()
{- execute program, prints rv register on screen in case of termination -}
runProgram file = do program <- loadFromFile file
                     print (interp program)


interp :: Program -> Int 
{- Runs the entire program, returns value in register rv -}
interp prog = runReg prog dataList


runReg :: Program -> [Int] -> Int
{- 
execute each instruction 
needs program, dataList, and returns integer
-}
runReg prog d = if length prog <= d !! index Index
                  then d !! index Rv
                  else let instruction = prog !! (d !! index Index)
                        in runReg prog (findInstruction instruction (increment d))
              

-- ACQUIRING DATA --
loadFromFile :: FilePath -> IO Program
{- 
 Takes the register program file name and returns
 IO action containing the program
-}
loadFromFile file = do content <- readFile file
                       return (parsing content)


--_________--
--  TYPES  --
--_________--
                    -- moving data
data Instruction =   Mov  Reg Reg
                    | Movc Reg Int
                    -- arithmetic
                    | Add  Reg Reg Reg
                    | Addc Reg Int
                    | Sub  Reg Reg Reg
                    | Subc Reg Int
                    | Mul  Reg Reg Reg
                    | Mulc Reg Int
                    | Div  Reg Reg Reg
                    | Divc Reg Int
                    -- jumping
                    | Jmp  Int
                    | Jal  Int
                    | Jif  Int Reg
                    | Jr   Reg
    deriving (Read, Show)


data Reg =    Rv        -- return value
            | Lnk       -- prior index value
            | R0
            | R1
            | R2
            | R3
            | R4
            | R5
            | R6
            | R7
            | R8
            | R9
            | R10
            | R11
            | R12
            | R13
            | R14
            | R15
            | Index     -- current line of program
    deriving (Read, Show)


type Program = [Instruction]


--___________--
-- FUNCTIONS --
--___________--

-- DEFINITIONS --
dataList :: [Int]
{- list of registers -}
dataList = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]


index :: Reg -> Int
{-
 sets numbers for each Reg;
 index into dataList by
 dataList !! index <register name>
-}
index Rv    = 0
index Lnk   = 1
index R0    = 2
index R1    = 3
index R2    = 4
index R3    = 5
index R4    = 6
index R5    = 7
index R6    = 8
index R7    = 9
index R8    = 10
index R9    = 11
index R10   = 12
index R11   = 13
index R12   = 14
index R13   = 15
index R14   = 16
index R15   = 17
index Index = 18


increment :: [Int] -> [Int]
{- inrements the Index counter -}
increment d = updateVal d (index Index, 
                           d !! index Index + 1)


-- INTERACTING WITH DATA --
parsing :: String -> Program
{- 
 convert String from loadFile into a list of Instruction 
 now the data is in this format:
 info = ["Movc R4 R2", "Sub R3 R5"]
-}
parsing content = [read x | x <- info]
    where info  = lines content


updateVal :: [a] -> (Int, a) -> [a]
{- 
 change a register via indexing;
 takes: dataList (index, what to put there).
-} 
updateVal [] _         = []
updateVal (_:xs) (0,a) = a:xs
updateVal (x:xs) (n,a) = x:updateVal xs (n-1, a)


findInstruction :: Instruction -> [Int] -> [Int]
{-
 calls the given function when it meets
 something it recognizes.
 when using defInst. call it with dataList.
-}

-- move --
findInstruction (Mov  arg1 arg2)      = move (Mov  arg1 arg2)
findInstruction (Movc arg1 arg2)      = move (Movc arg1 arg2)

-- arithmetic --
findInstruction (Add  arg1 arg2 arg3) = arithmetic (Add  arg1 arg2 arg3)
findInstruction (Addc arg1 arg2)      = arithmetic (Addc arg1 arg2)
findInstruction (Sub  arg1 arg2 arg3) = arithmetic (Sub  arg1 arg2 arg3)
findInstruction (Subc arg1 arg2)      = arithmetic (Subc arg1 arg2)
findInstruction (Mul  arg1 arg2 arg3) = arithmetic (Mul  arg1 arg2 arg3)
findInstruction (Mulc arg1 arg2)      = arithmetic (Mulc arg1 arg2)
findInstruction (Div  arg1 arg2 arg3) = arithmetic (Div  arg1 arg2 arg3)
findInstruction (Divc arg1 arg2)      = arithmetic (Divc arg1 arg2)

-- jumping --
findInstruction (Jmp  arg1)           = jumping (Jmp arg1)
findInstruction (Jal  arg1)           = jumping (Jal arg1)
findInstruction (Jif  arg1 arg2)      = jumping (Jif arg1 arg2)
findInstruction (Jr   arg1)           = jumping (Jr  arg1)


move :: Instruction -> [Int] -> [Int]
{- 
 wherein d is the given list (dataList) 
 and the result is in the last listed register
-}
move (Mov  arg1 arg2) d     = updateVal d (index arg2,
                                           d !! index arg1)
move (Movc arg1 arg2) d     = updateVal d (index arg1, arg2)


arithmetic :: Instruction -> [Int] -> [Int]
{-
 instructions put results in the last listed register,
 i.e arg3 for Add, but arg1 for Addc.
-}
arithmetic (Add  arg1 arg2 arg3) d = updateVal d (index arg3,
                                                  d !! index arg1 +
                                                  d !! index arg2)

arithmetic (Addc arg1 arg2)      d = updateVal d (index arg1,
                                                  d !! index arg1 + 
                                                  arg2)

arithmetic (Sub  arg1 arg2 arg3) d = updateVal d (index arg3,
                                                  d !! index arg1 -
                                                  d !! index arg2)

arithmetic (Subc arg1 arg2)      d = updateVal d (index arg1,
                                                  d !! index arg1 - 
                                                  arg2)

arithmetic (Mul  arg1 arg2 arg3) d = updateVal d (index arg3,
                                                  d !! index arg1 *
                                                  d !! index arg2)

arithmetic (Mulc arg1 arg2)      d = updateVal d (index arg1,
                                                  d !! index arg1 * 
                                                  arg2)

arithmetic (Div  arg1 arg2 arg3) d = updateVal d (index arg3,
                                                  d !! index arg1 `div`
                                                  d !! index arg2)

arithmetic (Divc arg1 arg2)      d = updateVal d (index arg1,
                                                  d !! index arg1 `div` 
                                                  arg2)


jumping :: Instruction -> [Int] -> [Int]
jumping (Jmp arg1)      d = updateVal d (index Index, arg1)

jumping (Jal arg1)      d = updateVal (updateVal d (index Lnk, 
                                                    d !! index Index))
                                      (index Index, arg1)

jumping (Jif arg1 arg2) d = if d !! index arg2 /= 0
                             then jumping (Jmp arg1) d
                             else d

jumping (Jr  arg1)      d =  jumping (Jmp (d !! index arg1)) d


{-
FOR RUNNING PROGRAM
    ** be in the same directory as file
    hlint Exam.hs

    ** or in general
    hlint filename

    ** loading file
    ghci
    :load filename
    or
    :l filename

    and then ofc to run a program:
    runProgram filepath

    ** running testsuite
    functionname 

-}