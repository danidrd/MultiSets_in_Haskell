module Main where

import MultiSet

main :: IO ()
main = do
    -- Define some test multisets
    let ms1 = MS [("a", 1), ("b", 2), ("c", 3)]
    let ms2 = MS [("b", 1), ("c", 3), ("d", 4)]
    let ms3 = MS [("a", 1), ("b", 2)]
    let ms4 = MS [("a", 1), ("b", 3), ("c", 3)]

    -- Testing add function
    assert (add ms1 "b" == MS [("a", 1), ("b", 3), ("c", 3)]) "add test 1 failed"
    assert (add ms3 "d" == MS [("a", 1), ("b", 2), ("d", 1)]) "add test 2 failed"

    -- Testing occs function
    assert (occs ms1 "b" == 2) "occs test 1 failed"
    assert (occs ms1 "d" == 0) "occs test 2 failed"

    -- Testing elems function
    assert (elems ms1 == ["a", "b", "b", "c", "c", "c"]) "elems test 1 failed"
    assert (elems ms3 == ["a", "b", "b"]) "elems test 2 failed"

    -- Testing subeq function
    assert (subeq ms3 ms1) "subeq test 1 failed"
    assert (not (subeq ms1 ms3)) "subeq test 2 failed"

    -- Testing union function
    assert (union ms1 ms2 == MS [("a", 1), ("b", 3), ("c", 6), ("d", 4)]) "union test 1 failed"
    assert (union ms3 ms3 == MS [("a", 2), ("b", 4)]) "union test 2 failed"

    -- If all tests pass
    putStrLn "All tests passed!"

-- Helper function to assert test results
assert :: Bool -> String -> IO ()
assert True _    = return ()
assert False msg = error msg
