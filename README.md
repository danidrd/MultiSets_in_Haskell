# MultiSet Haskell Module

## Description

The `MultiSet` module provides an implementation of multisets (sets that allow duplicate elements) in Haskell. It includes functions to perform various operations on multisets, such as adding elements, counting occurrences, extracting elements, checking subset relations, computing unions and check same elements between Multisets.

The module ensures that all operations return well-formed multisets, where:
- All multiplicities are positive.
- There are no duplicate elements (all elements are distinct).

A `main` program is included to test the functionalities provided by the module.
A `test` program is included to test also read and write from/to files. (token = \n) => if you want to change the files in input,
make sure that the file in question is well formatted (only a word for each line).

---

## Features

The `MultiSet` module provides the following functionalities:

1. **Add an Element**: Adds an element to a multiset, increasing its multiplicity if it already exists.
2. **Count Occurrences**: Counts the occurrences of a given element in the multiset.
3. **Extract Elements**: Extracts all elements of the multiset as a list, duplicating each element based on its multiplicity.
4. **Check Subset Relation**: Determines if one multiset is a subset of another.
5. **Union**: Computes the union of two multisets, summing the multiplicities of shared elements.
6. **Ensure Well-Formedness**: All operations guarantee the result is a well-formed multiset.
7. **Check Same Elements**: Determines if one multiset has the same elements (without considering multiplicities) of another.

---

## Installation

1. Clone this repository to your local machine:
   ```bash
   git clone https://github.com/danidrd/MultiSets_in_Haskell.git
   cd MultiSets_in_Haskell
2. Ensure you have GHC (Glasgow Haskell Compiler) installed. If not, you can download and install it from the official website: https://www.haskell.org/ghc/
3. Compile the Haskell source code:
   ```bash
   ghc -o main main.hs MultiSet.hs
   ghc -o test TestMSet.hs MultiSet.hs
4. Run the compiled program:
   ```bash
   ./test
   ./main
   ```
   **In alternative**:
   ```bash
   runghc TestMSet.hs
   runghc main.hs
5. Check the results with checker.awk
   ```bash
   chmod 755 checker.awk
   ./checker.awk gana-out.txt anag-out.txt 

