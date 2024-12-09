{-******************************************
*   Exam.hs                    05.01.25    *
*        Author: Sandra K. Johansen        *
*******************************************-}

--____________--
-- GROUNDWORK --
--____________--

-- RUN PROGRAM --
runProgram :: FilePath -> IO ()
{- execute program, prints rv register on screen in case of termination -}
runProgram file = interp (loadFromFile file)

interp :: Program -> Int 
{- Runs the entire program, returns value in register rv -}


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
data Instructions =   Mov  Reg Reg
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

type Program = [Instructions]

data Reg =    Rv        -- return value
            | Lnk       -- prior index value
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
            | R12
            | R13
            | R14
            | R15
            | R16
            | Index     -- current line of program
    deriving (Read, Show)


--___________--
-- FUNCTIONS --
--___________--

-- DEFINITIONS --
dataList :: [Int]
{- list of registers -}
dataList = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

index :: Reg -> Int
{-
 sets numbers for each Reg;
 index into dataList by
 dataList !! index R2
-}
index Rv    = 0
index Lnk   = 1
index R1    = 2
index R2    = 3
index R3    = 4
index R4    = 5
index R5    = 6
index R6    = 7
index R7    = 8
index R8    = 9
index R9    = 10
index R10   = 11
index R11   = 12
index R12   = 13
index R13   = 14
index R14   = 15
index R15   = 16
index R16   = 17
index Index = 18


whoops :: IO ()
{- raises error if attempt at non-existing memory -}
whoops = error "Whoops! You're not allowed to go there."

increment :: [a] -> [a]
increment d = replace d (index Index, ((d !! index Index) + 1))


-- INTERACTING WITH DATA --
line :: [a] -> Int
{- find current line of program -}
line d = d !! index Index 

parsing :: String -> Program
{- 
 convert String from loadFile into a list of Instructions 
 now the data is in this format:
 info = ["Movc R4 R2", "Sub R3 R5"]
-}
parsing content = [read x | x <- info]
    where info  = lines content
-- with [read x | x <- info] haskell by itself can find and read the types


replace ::[a] -> (Int, a) -> [a]
{- 
 found the basis for this function online. 
 needed something to change a register
 takes list, index, and what it needs to put there.
-} 
replace [] _        = []
replace (_:xs) (0,a) = a:xs
replace (x:xs) (n,a) = x: replace xs (n-1, a)


defInstruction :: Instructions -> [a] -> [a]
{-
 calls the given function when it meets
 something it recognizes.
 when using defInst. call it with dataList.
-}

-- move --
defInstruction (Mov arg1 arg2)      = move (Mov arg1 arg2)
defInstruction (Movc arg1 arg2)     = move (Movc arg1 arg2)

-- arithmetic --
defInstruction (Add arg1 arg2 arg3) = arithmetic (Add arg1 arg2 arg3)
defInstruction (Addc arg1 arg2)     = arithmetic (Addc arg1 arg2)
defInstruction (Sub arg1 arg2 arg3) = arithmetic (Sub arg1 arg2 arg3)
defInstruction (Subc arg1 arg2)     = arithmetic (Subc arg1 arg2)
defInstruction (Mul arg1 arg2 arg3) = arithmetic (Mul arg1 arg2 arg3)
defInstruction (Mulc arg1 arg2)     = arithmetic (Mulc arg1 arg2)
defInstruction (Div arg1 arg2 arg3) = arithmetic (Div arg1 arg2 arg3)
defInstruction (Divc arg1 arg2)     = arithmetic (Divc arg1 arg2)

-- jumping --
defInstruction (Jmp arg1)           = jumping (Jmp arg1)
defInstruction (Jal arg1)           = jumping (Jal arg1)
defInstruction (Jif arg1)           = jumping (Jif arg1)
defInstruction (Jr arg1)            = jumping (Jr arg1)


move :: Instructions -> [a] -> [a]
{- wherein d is the given list (dataList) -}
move (Mov arg1 arg2)  d     = replace d (index arg2,
                                      d !! (index arg1))
move (Movc arg1 arg2) d     = replace d (index arg1, arg2)


arithmetic :: Intructions -> [a] -> [a]
{-
instructions put results in the last listed register,
i.e arg3 for Add, but arg1 for Addc.
-}
arithmetic (Add arg1 arg2 arg3) d = replace d (index arg3,
                                           (d !! (index arg1) +
                                           (d !! (index arg2))))
arithmetic (Addc arg1 arg2)     d = replace d (index arg1,
                                           (d !! (index arg1) + 
                                            arg2))

arithmetic (Sub arg1 arg2 arg3) d = replace d (index arg3,
                                           (d !! (index arg1) -
                                           (d !! (index arg2))))
arithmetic (Subc arg1 arg2)     d = replace d (index arg1,
                                           (d !! (index arg1) - 
                                            arg2))

arithmetic (Mul arg1 arg2 arg3) d = replace d (index arg3,
                                           (d !! (index arg1) *
                                           (d !! (index arg2))))
arithmetic (Mulc arg1 arg2)     d = replace d (index arg1,
                                           (d !! (index arg1) * 
                                            arg2))

arithmetic (Div arg1 arg2 arg3) d = replace d (index arg3,
                                           (d !! (index arg1) `div`
                                           (d !! (index arg2))))
arithmetic (Divc arg1 arg2)     d = replace d (index arg1,
                                           (d !! (index arg1) `div` 
                                            arg2))


jumping :: Instructions -> [a] -> [a]
jumping (Jmp arg1) d      = replace d (index Index, arg1)

jumping (Jal arg1) d      = replace (replace d (index Lnk, 
                                     (d !! index Index)))
                                    (index Index, arg1)

jumping (Jif arg1 arg2) d = if d !! index arg2 /= 0
                                then jumping (Jmp arg1) d
                                else increment d

jumping (Jr arg1)  d      = if (d !! index arg1) > 1 &&
                               (d !! index arg1) < 19
                                then jumping (Jmp (d !! index arg1)) d
                                else whoops  -- raise error
