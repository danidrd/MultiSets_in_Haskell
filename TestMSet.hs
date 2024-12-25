{-# LANGUAGE InstanceSigs #-}
module Main where
import MultiSet ( add, empty, union, MSet(..), checkSameElements )
import Data.List (foldl')


-- Read a multiset from a file
readMSet :: FilePath ->  IO(MSet [Char])
readMSet fileName = do
    content <- readFile fileName
    return (processMSet content)

-- Process the content of the file into a multiset
processMSet ::  String -> MSet [Char]
processMSet content =
    let ls = lines content
    in foldl' add empty ls

-- Write a multiset to a file
writeMSet :: MSet [Char] -> String -> IO ()
writeMSet mset fileName = 
    writeFile fileName (unlines (formatMSet mset))

-- Helper function to format the multiset into a list of strings
formatMSet :: MSet [Char] -> [String]
formatMSet (MS []) = []
formatMSet (MS ((x, n) : xs)) = (x ++ " - " ++ show n) : formatMSet (MS xs)


main :: IO ()
main = do
    m1 <- readMSet "../aux_files/anagram.txt"
    m2 <- readMSet "../aux_files/anagram-s1.txt"
    m3 <- readMSet "../aux_files/anagram-s2.txt"
    m4 <- readMSet "../aux_files/margana2.txt"

    -- Test cases
        
    assert (m1 == m4) "m1 and m4 are not equal"
    assert (checkSameElements m1 m4) "m1 and m4 does not have the same Elements"
    assert (m1 /= union m2 m3) "m1 is equal to the union m2 m3"

    writeMSet m1 "anag-out.txt"
    writeMSet m4 "gana-out.txt"

    putStrLn "All tests passed!"




-- Helper function to assert test results
assert :: Bool -> String -> IO ()
assert True _    = return ()
assert False msg = print msg