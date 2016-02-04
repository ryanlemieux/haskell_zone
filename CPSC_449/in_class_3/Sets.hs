{-
 - CPSC 449 :: Programming Paradigms
 - Winter 2016 - L01
 -
 - In-Tutorial (Class) Exercise 3
 -
 - Passes all tests (32/32).
 -
 -}

module Sets where
import System.Environment

{-
 - isSet:
 -
 - This function looks at a list and returns a Bool type indicating whether or not
 - it is a "set" (in this case, defined as having no duplicate elements).
 -
 - There are three "guards" (indicated with the pipe ("|") character, AKA conditions):
 -
 - Essentially, it keeps taking the first element of the list (the "head") and checking
 - to see if its in the tail of the list. As soon as it finds a duplicate element, it
 - returns False.
 -
 - Otherwise, it returns True.
 -}

isSet :: Eq a => [a] -> Bool
isSet list
	| length list > 1 && (head list) `elem` (tail list) = False
	| length list > 1 && not ((head list) `elem` (tail list)) = isSet (tail list)
	| otherwise = True

{-
 - toSet:
 -
 - This function accepts a list, and passes it to the "dedupe" function, which removes
 - all duplicate elements.
 -}

toSet :: Eq a => [a] -> [a]
toSet list = dedupe list []

{-
 - elementOf:
 -
 - This function accepts an "item" (some arbitrary type) and a list (containing elements
 - of some arbitrary type), and returns an Either type (a union).
 -
 - There are three "guards" (indicated with the pipe ("|") character, AKA conditions):
 -
 - Right, a Bool type, is returned if the list is a set (contains no duplicates);
 - the Bool indicates whether or not the item passed to the function is an element of
 - the set passed to the function.
 -
 - Left, a String type, is returned if the list passed to the function is not a set
 - (it contains duplicates). The output string also includes the first duplicate
 - element in the set, which is returned from the function 'firstDupe'.
 -
 - Important note: The "show" function is necessary for concatenating the output
 - of "firstDupe" with the Left string; it returns a String object from arbitrary type,
 - if the type has a "show" defined.
 -
 - Another important note: The string that follows Left must be encapsulated in
 - parentheses, as Left expects only one argument. Without the parentheses, the
 - string concatenation (++ operators) actually make the string multiple arguments.
 -}

elementOf :: (Eq a, Show a) => a -> [a] -> Either String Bool
elementOf item list
	| isSet list && item `elem` list = Right True
	| isSet list && not (item `elem` list) = Right False
	| otherwise = Left ("The 2nd argument is not a set; the element " ++ show (firstDupe(list)) ++ " is duplicated.")

{-
 - intersection:
 -
 - This function accepts two lists (of arbitrary types) and returns a Maybe type.
 - The Just output of Maybe is a list that is the intersection of the two input
 - lists (all elements that are common to both). If one or both of the input lists
 - are not "sets" (i.e. they have duplicate elements), then Nothing is returned.
 -
 - This function calls the function "allDupes", which returns a list of all elements
 - common to both lists.
 -}

intersection :: Eq a => [a] -> [a] -> Maybe [a]
intersection setA setB
	| isSet setA && isSet setB = Just (allDupes setA setB [])
	| not (isSet setA) || not (isSet setB) = Nothing

{-
 - allDupes:
 -
 - This function returns a list of all elements that are common to both of the input
 - lists. It is passed an empty list in which to place these duplicate elements.
 -
 - It works by taking elements one at a time from setA, and checking if they are in
 - setB. If the element is found in setB, the element is added to dupeSet, and allDupes
 - is recursively called.
 -
 - If the element is not found in setB, allDupes is recursively called without modification
 - of dupeSet.
 -}

allDupes :: Eq a => [a] -> [a] -> [a] -> [a]
allDupes setA setB dupeSet
	| length setA > 0 && (head setA) `elem` setB = allDupes (tail setA) setB ((head setA):dupeSet)
	| length setA > 0 && not ((head setA) `elem` setB) = allDupes (tail setA) setB dupeSet
	| length setA == 0 = dupeSet

{-
 - firstDupe:
 -
 - This function accepts a list (of some arbitrary type), and returns the first duplicate element it finds in
 - the list, starting from the beginning of the list.
 -
 - There are three "guards" (indicated with the pipe ("|") character, AKA conditions):
 -
 - The first condition checks to see if the first element in the list (the "head") is
 - duplicated in the rest of the list (the "tail"). If it is, that element is returned
 - by the function.
 -
 - The second condition checks to see if the list has more than 1 element in it,
 - and if so, if the "head" is NOT duplicated in the "tail". If it is not, then the
 - function is recursively called, with the "tail" of the list being passed as the
 - argument.
 -
 - The third condition simply outputs an error if the input list is in fact a "set",
 - i.e. it contains no duplicates.
 -}

firstDupe :: Eq a => [a] -> a
firstDupe list
	| (head list) `elem` (tail list) = head list
	| length list > 1 && not ((head list) `elem` (tail list)) = firstDupe (tail list)
	| otherwise = error "This is a set (no duplicates), or the list is empty."

{-
 - dedupe:
 -
 - This function accepts two arguments: a list (that is to have its duplicates pruned),
 - and an empty list. The empty list is populated with all of the elements of the first
 - list, minus any duplicate elements.
 -
 - This function consists of two if statements (nested).
 -
 - The first checks to see if the input list that is to have its duplicates pruned
 - is empty. If it is empty, the function returns an empty list. This check is important,
 - because calling "head" / "tail" on an empty list causes an error (I believe?).
 -
 - The function is of course recursive, and operates as such:
 -
 - Take the first element of the list, and compare it with the rest of the list.
 - If the element is not found in the rest of the list, add it to the newList,
 - and call dedupe recursively. If it's a duplicate element, call the function
 - recursively without adding it to the newList.
 -
 - Important note:
 -
 - The way that the "then" branch of the nested if statement is called is constructed
 - such that two "actions" occur: the non-duplicate element is added to newList, and
 - dedupe is called recursively.
 -
 - Initially I found this confusing, and the way my mind wanted to write it was by
 - first appending the non-duplicate item to newList, then passing list and newList
 - to the recursive call. This isn't possible in Haskell.
 -
 - The correct way to think about doing these two "actions" at once, is by realizing
 - that the cons operater (":", which appends an element to the front of a list in
 - constant time) does not actually modify newList in memory: it takes an element
 - and a list, and returns a NEW list.
 -
 - Thus, the proper way to do both of those "actions" at once in Haskell, is to
 - pass the return value of the cons function to the input of the recursive call.
 -
 - I hope this insight is valuable; for me understanding this was a major "aha"
 - moment with regards to thinking in the functional paradigm.
 -}

dedupe :: Eq a => [a] -> [a] -> [a]
dedupe list newList = if length list > 0
			then
				if not ((head list) `elem` (tail list))
				then dedupe (tail list) ((head list):newList)
				else dedupe (tail list) newList
			else newList

