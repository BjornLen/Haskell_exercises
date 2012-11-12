module Pattern where
import Utilities


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute w t s = concat [replace w x s | x <- t]
	where replace wild sub val
		| sub == wild = val
		| otherwise = [sub]
-- TODO add empty list cases

-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match w [] [] = Just []
match w [] (s:ss) = Nothing
match w (t:ts) [] = Nothing
match w (t:ts) (s:ss)
	| t == s = match w ts ss
--	| t == w = orElse (singleWildcardMatch t:ts s:ss) ( longerWildcardMatch t:ts s:ss)
	| t == w =(singleWildcardMatch t:ts s:ss) 
	| otherwise = Nothing

-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs)
	| match wc ps xs /= Nothing = Just [x]
	| otherwise = Nothing

longerWildcardMatch (wc:ps) (x:xs) = Nothing
{- TO BE WRITTEN -}



-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions



-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply _ _ _ _ = Nothing
{- TO BE WRITTEN -}


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ _ _ = Nothing
{- TO BE WRITTEN -}
