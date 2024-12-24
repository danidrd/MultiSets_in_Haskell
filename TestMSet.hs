{-# LANGUAGE InstanceSigs #-}
module Main where
import MultiSet
import Type.Reflection
import Control.DeepSeq

instance NFData a => NFData (MSet a) where
    rnf :: NFData a => MSet a -> ()
    rnf (MS xs) = rnfPairs xs
        where
            -- Force valutation of each pair
            rnfPairs :: NFData a => [(a, Int)] -> ()
            rnfPairs [] = ()
            rnfPairs ((x, n) : xs') = rnf x `seq` rnf n `seq` rnfPairs xs'


-- Impure operation, cannot unwrap MSet String so we use processMSet for doing that
readMSet :: FilePath -> IO ( MSet [Char] )
readMSet fileName = do
    content <- readFile fileName
    let mset = processMSet content
    mset `deepseq` return mset

processMSet :: String -> MSet [Char]
processMSet content =
    let ls = lines content
        in foldl add empty ls

writeMSet :: MSet [Char] -> String -> IO ()
writeMSet mset fileName =
    writeFile fileName (unlines (elems mset))

main :: IO ()
main = do
    m1 <- readMSet "../aux_files/anagram.txt"
    m2 <- readMSet "../aux_files/anagram-s1.txt"
    m3 <- readMSet "../aux_files/anagram-s2.txt"
    m4 <- readMSet "../aux_files/margana2.txt"
    printType m1
    printType m4
        
    assert (m1 == m4) "Test 1 Failed"
    assert (m1 == union m2 m3) "Test 2 Failed"

    writeMSet m1 "anag-out.txt"
    writeMSet m4 "gana-out.txt"

    putStrLn "All tests passed!"


printType :: Typeable a => a -> IO ()
printType x = print $ typeOf x

-- Helper function to assert test results
assert :: Bool -> String -> IO ()
assert True _    = return ()
assert False msg = error msg