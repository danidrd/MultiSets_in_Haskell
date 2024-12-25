{-# LANGUAGE InstanceSigs #-}
module Main where
import MultiSet
import Data.List (foldl')


-- Impure operation, cannot unwrap MSet String so we use processMSet for doing that
readMSet :: FilePath ->  IO(MSet [Char])
readMSet fileName = do
    content <- readFile fileName
    return (processMSet content)

processMSet ::  String -> MSet [Char]
processMSet content =
    let ls = lines content
    in foldl' add empty ls

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
        
    assert (m1 /= m4) "Test 1 Failed"
    assert (m1 == union m2 m3) "Test 2 Failed"

    writeMSet m1 "anag-out.txt"
    writeMSet m4 "gana-out.txt"

    putStrLn "All tests passed!"


-- Helper function to assert test results
assert :: Bool -> String -> IO ()
assert True _    = return ()
assert False msg = error msg