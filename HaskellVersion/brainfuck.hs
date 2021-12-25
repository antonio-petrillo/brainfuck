import Data.Char

dataMemory :: [Int]
dataMemory = replicate 30000 0

-- print A read a byte, print that byte
program :: String
program = "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++.,."

hello :: String
hello = ">++++++++[<+++++++++>-]<.>++++[<+++++++>-]<+.+++++++..+++.>>++++++[<+++++++>-]<++.------------.>++++++[<+++++++++>-]<+.<.+++.------.--------.>>>++++[<++++++++>-]<+."

executeProgram :: (String, Int) -> ([Int], Int) -> [Int] -> IO ()
executeProgram (program, ic) (memory, pointer) stack = if length program == ic then
                                                         return ()
                                                       else
                                                         case program !! ic of
                                                           '+' -> executeProgram (program, ic + 1) (incrementMemory memory pointer, pointer) stack
                                                           '-' -> executeProgram (program, ic + 1) (decrementMemory memory pointer, pointer) stack
                                                           '.' -> do
                                                                    putChar (chr $ memory !! pointer)
                                                                    executeProgram (program, ic + 1) (memory, pointer) stack
                                                           ',' -> do
                                                                    c <- getChar
                                                                    let byte = ord c
                                                                    executeProgram (program, ic + 1) (updateDataAtPointBy memory pointer byte, pointer) stack
                                                           '[' ->  if memory !! pointer == 0 then
                                                                     executeProgram (program, ic') (memory, pointer) stack
                                                                   else
                                                                     executeProgram (program, ic + 1) (memory, pointer) ([ic] ++ stack)
                                                           ']' -> if memory !! pointer /= 0 then
                                                                    executeProgram (program, head stack ) (memory, pointer) $ tail stack
                                                                  else
                                                                    executeProgram (program, ic + 1) (memory, pointer) stack
                                                           '>' ->  executeProgram (program, ic + 1) (memory, (pointer + 1) `mod` 30000) stack
                                                           '<' ->  executeProgram (program, ic + 1) (memory, (pointer - 1) `mod` 30000) stack
                                                where
                                                  ic' = goForward program ic 0

goForward :: String -> Int -> Int -> Int
goForward program ic numOfBracket = if program !! ic == ']' && numOfBracket == 0 then
                                      ic
                                    else
                                      goForward program (ic + 1) numOfBracket'
                                where
                                  numOfBracket' = if program !! ic == '[' then numOfBracket + 1 else if program !! ic == ']' then numOfBracket - 1 else numOfBracket


incrementMemory :: [Int] -> Int -> [Int]
incrementMemory memory pointer = if pointer == 0 then
                                   [head memory + 1] ++ tail memory
                                 else
                                   take pointer memory ++ [memory !! pointer + 1] ++ drop (pointer + 1) memory

decrementMemory :: [Int] -> Int -> [Int]
decrementMemory memory pointer = if pointer == 0 then
                                   [head memory - 1] ++ tail memory
                                 else
                                   take pointer memory ++ [memory !! pointer - 1] ++ drop (pointer + 1) memory

updateDataAtPointBy :: [Int] -> Int -> Int -> [Int]
updateDataAtPointBy memory pointer byte = if pointer == 0 then
                                            [byte] ++ tail memory
                                          else
                                            take pointer memory ++ [byte] ++ drop (pointer + 1) memory

main :: IO ()
main = executeProgram (hello, 0) (dataMemory, 0) []
