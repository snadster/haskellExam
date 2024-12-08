{-******************************************
*   Exam.hs                    05.01.25    *
*        Author: Sandra K. Johansen        *
*******************************************-}

-- TYPES
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
                    | Jif  Reg Int
                    | Jr   Reg
    deriving (Read, Show)

type Program = [Instructions]

data Reg =    Rv        -- return value
            | Lnk       -- keep index value
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
    deriving (Read, Show)


--___________--
-- FUNCTIONS --
--___________--

funcName :: Program -> NULL
-- need tp actually run through list of instructions,
-- so far we are NOT doing that. whoopsie.

interp :: Program -> Int 
-- Runs the entire program, returns value in register rv


-- DEFINITIONS --
dataList :: [Int]
-- list of registers
dataList = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

regToList :: Reg -> Int
-- index into dataList by:
-- dataList !! regToList R2
regToList Rv  = 0
regToList Lnk = 1
regToList R1  = 2
regToList R2  = 3
regToList R3  = 4
regToList R4  = 5
regToList R5  = 6
regToList R6  = 7
regToList R7  = 8
regToList R8  = 9
regToList R9  = 10
regToList R10 = 11
regToList R11 = 12
regToList R12 = 13
regToList R13 = 14
regToList R14 = 15
regToList R15 = 16
regToList R16 = 17


parsing :: String -> Program
-- convert String from loadFile into a list of Instructions
parsing content = [read x | x <- info]
    where info  = lines content

-- now the data is in this format:
-- info = ["Movc R4 R2", "Sub R3 R5"]
-- with [read x | x <- info] it by itself can find and read the types


replace ::[a] -> (Int, a) -> [a]
-- found the basis for this function online. 
-- needed something to change a register
-- takes list, index, and what it needs to put there.
replace [] _        = []
replace (_:xs) (0,a) = a:xs
replace (x:xs) (n,a) = x: replace xs (n-1, a)


defInstruction :: Instructions -> [a] -> [a]
{-
calls the given function when it meets
something it recognizes.
-}
-- when using defInst. call it with dataList.

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
-- wherein d is the given list (dataList)
move (Mov arg1 arg2)  d     = replace d (regToList arg2,
                                      d !! (regToList arg1))
move (Movc arg1 arg2) d     = replace d (regToList arg1, arg2)


arithmetic :: Intructions -> [a] -> [a]
{-
instructions put results in the last listed register,
i.e arg3 for Add, but arg1 for Addc.
-}
arithmetic (Add arg1 arg2 arg3) d = replace d (regToList arg3,
                                           (d !! (regToList arg1) +
                                           (d !! (regToList arg2))))
arithmetic (Addc arg1 arg2)     d = replace d (regToList arg1,
                                           (d !! (regToList arg1) + 
                                            arg2))

arithmetic (Sub arg1 arg2 arg3) d = replace d (regToList arg3,
                                           (d !! (regToList arg1) -
                                           (d !! (regToList arg2))))
arithmetic (Subc arg1 arg2)     d = replace d (regToList arg1,
                                           (d !! (regToList arg1) - 
                                            arg2))

arithmetic (Mul arg1 arg2 arg3) d = replace d (regToList arg3,
                                           (d !! (regToList arg1) *
                                           (d !! (regToList arg2))))
arithmetic (Mulc arg1 arg2)     d = replace d (regToList arg1,
                                           (d !! (regToList arg1) * 
                                            arg2))

arithmetic (Div arg1 arg2 arg3) d = replace d (regToList arg3,
                                           (d !! (regToList arg1) `div`
                                           (d !! (regToList arg2))))
arithmetic (Divc arg1 arg2)     d = replace d (regToList arg1,
                                           (d !! (regToList arg1) `div` 
                                            arg2))


jumping :: Instructions -> [a] -> [a]
jumping (Jmp arg1) d = info !! (regToList arg1)
jumping (Jal arg1) d = info !! (regToList arg1) && replace d (1)
jumping (Jif arg1) d = 
jumping (Jr arg1)  d =


loadFromFile :: FilePath -> IO Program
-- Takes the register program file name and returns
-- IO action containing the program
-- i think it returns a program (a list of instructions)

loadFromFile file = do content <- readFile file
                       return (parsing content)


runProgram :: FilePath -> IO ()
-- execute program, prints rv register on screen in case of termination.


