{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Redundant where" #-}


module MultiSet(
    MSet(..), -- Export both the type `MS` abd the constructor `MS`
    empty,
    add,
    occs,
    elems,
    subeq,
    union,
    checkSameElements,
) where
data MSet a = MS [(a, Int)]
    deriving (Show)

-- Check if a multiset is well-formed (i.e., all multiplicities are positive) and
-- all elements are distinct
well_formed :: Eq a => MSet a -> Bool
well_formed (MS []) = True
well_formed (MS ((x, n) : xs))
    | n <= 0 = False
    | any (\(y, _) -> x == y) xs = False
    | otherwise = well_formed (MS xs)

-- Ensure that a multiset is well-formed AFTER performing operations
ensureWellFormed :: Eq a => MSet a -> MSet a
ensureWellFormed ms
    | well_formed ms = ms
    | otherwise = error "The multiset is not well-formed."


-- Implementation of Eq instance for MSet
instance Eq a => Eq (MSet a) where
    (==) :: Eq a => MSet a -> MSet a -> Bool
    (MS []) == (MS []) = True
    (MS []) == _       = False
    _ == (MS [])       = False
    (MS xs) == (MS ys) = sameContent xs ys && sameContent ys xs
      where
        -- Checks that all elements in xs are in ys with the same multiplicity
        sameContent :: Eq a => [(a, Int)] -> [(a, Int)] -> Bool
        sameContent [] _ = True
        sameContent ((x, n) : xs') ys =
            case removeElem x n ys of
                Just ys' -> sameContent xs' ys' -- Element found; check the rest
                Nothing  -> False               -- Element not found or multiplicity mismatch

        -- Finds an element in the list and removes it if multiplicities match
        removeElem :: Eq a => a -> Int -> [(a, Int)] -> Maybe [(a, Int)]
        removeElem _ _ [] = Nothing
        removeElem x n ((y, m) : ys)
            | x == y && n == m = Just ys -- Exact match, remove it
            | x == y && n /= m = Nothing -- Match found, but multiplicity mismatch
            | otherwise = case removeElem x n ys of
                Just ys' -> Just ((y, m) : ys') -- Rebuild list without the matched element
                Nothing  -> Nothing             -- No match found



-- Implementation of Foldable instance for MSet
-- Minimal set of functuons implemented:
-- foldMap, foldr
-- both works only on the first element of each pairs within an MSet
instance Foldable MSet where
    foldMap :: Monoid m => (a -> m) -> MSet a -> m
    foldMap f (MS []) = mempty
    foldMap f (MS ((x, n) : xs)) = f x <> foldMap f (MS xs)


    foldr :: (a -> b -> b) -> b -> MSet a -> b
    foldr f acc (MS []) = acc
    foldr f acc (MS ((x, n) : xs)) = f x (foldr f acc (MS xs))

    foldl :: (b -> a -> b) -> b -> MSet a -> b
    foldl f acc (MS []) = acc
    foldl f acc (MS ((x, n) : xs)) = foldl f (f acc x) (MS xs)

-- Implementation of the module MultiSet

-- Empty Constructor
empty :: MSet a
empty =  MS [];

-- Adds an element v to the multiset
add :: Eq a => MSet a -> a -> MSet a
add (MS []) v = ensureWellFormed $ MS [(v, 1)]
add (MS ((x, n) : xs)) v
    | x == v    =  ensureWellFormed $ MS ((x, n + 1) : xs)
    | otherwise =  MS ((x, n) : elems')
  where
    MS elems' = add (MS xs) v



-- Count the occurences of an element in the multiset
occs :: Eq a => MSet a -> a -> Int
occs (MS []) v = 0
occs  (MS ((x, n) : xs)) v
    | x == v    = n
    | otherwise = occs (MS xs) v


-- Extract  as a list all elements from the multiset
elems :: MSet a -> [a]
elems (MS []) = []
elems (MS ((x, n) : xs)) = replicate n x ++ elems (MS xs)

-- Check if a multiset is a subset of another
subeq :: Eq a => MSet a -> MSet a -> Bool
subeq (MS []) (MS []) = True
subeq (MS []) _ = True
subeq _ (MS []) = False
subeq (MS ((x, n) : xs)) (MS ys) = check x n ys []
  where
    -- Auxilary function to check an element of MS1 against MS2
    check _ _ [] _ = False -- If nothing to check, return false
    check x n ((x', n') : xs') acc
        | x == x' && n >= n' = subeq (MS xs) (MS (acc ++ xs')) -- Element found, continue with MS1 reduced
        | otherwise          = check x n xs' (acc ++ [(x', n')]) -- Move the element in the last position and continue


-- Union of two multisets
union :: Eq a => MSet a -> MSet a -> MSet a
union (MS []) (MS []) = MS []
union (MS []) (MS ys) = MS ys
union (MS xs) (MS []) = MS xs
union (MS ((x, n) : xs)) (MS ys) =
   ensureWellFormed $ MS (combine ((x, n) : xs) ys)
  where
    -- Function for combining elements of MS1 and MS2
    combine :: Eq a => [(a, Int)] -> [(a, Int)] -> [(a, Int)]
    combine [] ys = ys
    combine ((x, n) : xs) ys =
        case lookup x ys of
            Just n' -> combine xs (removeOne x ys ++ [(x, n + n')])  -- Sum and remove the element from MS2
            Nothing -> (x, n) : combine xs ys -- No match, add x to the result

   -- Function for removing a specific element from a list
    removeOne :: Eq a => a -> [(a, Int)] -> [(a, Int)]
    removeOne _ [] = []
    removeOne x ((y, n) : ys)
        | x == y    = ys -- Remove only the first occurence
        | otherwise = (y, n) : removeOne x ys



-- Map a function over a multiset
-- Necessary to extract the inner list of the resulting MSet for avoiding
-- the type error, Mset b against [(b, Int)]
mapMSet :: (a -> b) -> MSet a -> MSet b
mapMSet f (MS []) = MS []
mapMSet f (MS ((x, n) : xs)) =
    let (MS xs') = mapMSet f (MS xs) -- Extract the inner list of the resulting MSet
    in MS ((f x, n) : xs')

-- Check if two multisets have the same elements with different multiplicities
checkSameElements :: Eq a => MSet a -> MSet a -> Bool
checkSameElements (MS []) (MS []) = True
checkSameElements (MS []) _ = False
checkSameElements _ (MS []) = False
checkSameElements (MS xs) (MS ys) = sameMS (MS xs) (MS ys) && sameMS (MS ys) (MS xs) 
    where
        sameMS :: Eq a => MSet a -> MSet a -> Bool
        sameMS (MS []) _ = True
        sameMS (MS ((x, _) : xs)) (MS ys) 
            | occs (MS ys) x >= 1 = sameMS (MS xs) (MS ys)
            | otherwise = False 


-- Why is not possible to use mapMSet as implementation of fmap for MSet istance of Functor?
-- for the Functor laws:
-- fmap id = id
-- fmap (f . g) = fmap f . fmap g

-- More in detail:
-- Violation of the identity laws, mapMSet does not inherently guarantee
-- that well-formedness of the resulting MSet, violating the expectded behavior.

-- Violations of the Composition laws(fmap (f . g) = fmap f . fmap g)
-- For example, if the function f . g maps multiple distinct elements to
-- the same element, the resulting multiset may not be well-formed (must handle the summing of the multiplicities)
-- For example:
-- ms = MS [(x, 1), (y, 2)]
-- f = const "z"
-- g = id

-- applying mapMSet (f . g) ms = MS [("z", 1), ("z", 2)] = MS [("z", 3)]
-- Applying mapMSet g and then mapMSet f:
-- mapMSet g ms = MS [(x, 1), (y, 2)]
-- mapMSet f (MS [(x, 1), (y, 2)]) = MS [("z", 1), ("z", 2)] = MS [("z", 3)]
-- it appears that composition holds in the first example, but the problem arises in non-well-formed multisets
-- where elements must be collapsed and their multiplicities summed.

-- Structural Issues with MSet as a Functor
-- The primary issue is that MSet is not just a container of elements ( [a] ),
-- but it also associates multiplicities ( Int ) with each element.

-- Applying a function to the fst of each (a, Int) pair can create duplicates in the fst
-- values that violate the well-formedness invariant of MSets.

-- Conclusion:
-- The reasons mapMSet cannot serve as fmap for a Functor instance of MSet are:
-- 1. Well-formedness: Applying a function to the fst of each pair in the MSet
-- can violate the well-formedness invariant
-- 2. Functor Laws: The functor laws (fmap id = id and fmap (f . g) = fmap f . fmap g)
-- depend on preserving structure, which is not guaranteed due to collapsing duplicates
-- and summing multiplicities. fmap id doesn't violate the invariant, but the composition
-- law (composing function different than id...) can be violated due to the collapsing and summing of multiplicities.
-- 3. Structural Complexity: The MSet is not a simple container like a list or tree;
-- it has additional constraints (e.g., unique keys and positive multiplicities) that
-- make it incompatible with the Functor interface.