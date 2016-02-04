module Main(main) where

import Control.Monad.Trans.State.Lazy
import Control.Exception
import Control.Monad
import Data.Maybe (fromJust, isNothing)
import Data.Foldable
import Data.List
import System.Exit
import System.Environment (getArgs,getProgName)
import System.IO.Unsafe
import Sets

---Main-------------------------------------------------------------

main = main' (unsafePerformIO getArgs)

{- | Contains list a list of tests that are evaluated, and the results summarized.
-}
main' :: [String] -> IO()
main' args = do
  printPasses <- interpretArgs args
  let
      -- This is just list of the tests.  They don't actually get evaluated until countM()
      tests = [
         (test (isSet [1,2,3])       (==) True  "isSet [1,2,3]")
        ,(test (isSet [1,1,2])       (==) False "isSet [1,1,2]")
        ,(test (isSet [1,2,2])       (==) False "isSet [1,2,2]")
        ,(test (isSet ([]::[Int]))   (==) True  "isSet []"      )
        ,(test (isSet [1.0,1.0,2.0]) (==) False "isSet [1.0,1.0,2.0]")
        ,(test (isSet [1.0,1.5,2.0]) (==) True  "isSet [1.0,1.5,2.0]")
        ,(test (isSet ["1","1","2"]) (==) False "isSet [\"1\",\"1\",\"2\"]")
        ,(test (isSet ["1","","2"])  (==) True  "isSet [\"1\",\"\",\"2\"]")

        ,(test (toSet [1,2,3])       (compareSets) [1,2,3]       "toSet [1,2,3]")
        ,(test (toSet [1,1,2])       (compareSets) [1,2]         "toSet [1,1,2]")
        ,(test (toSet [1,2,2])       (compareSets) [1,2]         "toSet [1,2,2]")
        ,(test (toSet ([]::[Int]))   (compareSets) []            "toSet []"      )
        ,(test (toSet [1.0,1.0,2.0]) (compareSets) [1.0,2.0]     "toSet [1.0,1.0,2.0]")
        ,(test (toSet [1.0,1.5,2.0]) (compareSets) [1.0,2.0,1.5] "toSet [1.0,1.5,2.0]")
        ,(test (toSet ["1","1","2"]) (compareSets) ["1","2"]     "toSet [\"1\",\"1\",\"2\"]")
        ,(test (toSet ["1","","2"])  (compareSets) ["1","","2"]  "toSet [\"1\",\"\",\"2\"]")

        ,(test (elementOf 1 [1,2,3])         (compareEither) (Right True)  "elementOf 1 [1,2,3]")
        ,(test (elementOf 1 [1,1,2])         (compareEither) (Left "")     "elementOf 1 [1,1,2]")
        ,(test (elementOf 1 [1,2,2])         (compareEither) (Left "")     "elementOf 1 [1,2,2]]")
        ,(test (elementOf 1 ([]::[Int]))     (compareEither) (Right False) "elementOf 1 ([]::[Int])")
        ,(test (elementOf 1.0 [1.0,1.0,2.0]) (compareEither) (Left "")     "elementOf 1.0 [1.0,1.0,2.0]")
        ,(test (elementOf 2.0 [1.0,1.5,2.0]) (compareEither) (Right True)  "2.0 [1.0,1.5,2.0]")
        ,(test (elementOf "3" ["1","2"])     (compareEither) (Right False) "elementOf \"3\" [\"1\",\"2\"]")
        ,(test (elementOf "" ["1","","2"])   (compareEither) (Right True)  "elementOf \"\" [\"1\",\"\",\"2\"]")

        ,(test (intersection [1,1,3] [2,3,4])  (==) Nothing                  "intersection [intersection [1,1,3] [2,3,4]")
        ,(test (intersection [1,2,3] [2,3,4])  (compareMaybe) (Just [2,3])   "intersection [1,2,3] [2,3,4]")
        ,(test (intersection [1,2] [3])        (compareMaybe) (Just [])      "intersection [1,2] [3]")
        ,(test (intersection ([]::[Int]) [])   (compareMaybe) (Just [])      "intersection ([]::[Int]) []")
        ,(test (intersection ([]::[Int]) [3])  (compareMaybe) (Just [])      "intersection ([]::[Int]) [3]"      )
        ,(test (intersection [1.0,1.5,2.0] [2.0,1.0,1.5]) (compareMaybe) (Just [1.0,2.0,1.5])  "intersection [1.0,1.5,2.0] [2.0,1.0,1.5]")
        ,(test (intersection ["1","2"] ["2"])  (compareMaybe) (Just ["2"])   "intersection [\"1\",\"2\"] [\"2\"]")
        ,(test (intersection ["1","","2"] [""]) (compareMaybe) (Just [""])   "intersection [\"1\",\"\",\"2\"] [\"\"]")
        ]
      in do gotRight <- countM printPasses tests
            -- Printing here has been moved to within countM() so that thinkgs get printed
            -- even when the program crashes part way though.
            --putStrLn $ ((foldr (\x -> (++)(format printPasses ((snd x)))) "" tests)::String)
            putStrLn $ "\nPassed " ++
                       (show gotRight) ++
                       "/" ++ (show (length tests)) ++
                       " tests.\nGrade: " ++ computeGrade (fromIntegral gotRight) (fromIntegral (length tests))

{- | Execute the tests (2nd param) and count the number of passed tests.  The tests
     should be executed within a catch context to try to catch (you can't catch them
     all in Haskell) errors in the target code.  As a side effect, the detail lines
     are printed here so that if the program bombs, we will at least have an idea what
     we bombed on.

     Parameters:
        1. Verbose: if true, print out ALL the test lines; if false the suppress the
           successful ones.
        2. The list of tests.
-}
countM :: Bool -> [(Bool, String)] -> IO Int
countM _ [] = return 0
countM verbose (x:xs) =
                do result <- Control.Exception.catch (return x) handler
                   putStr $ format verbose (snd result)
                   let res = case result of
                              (False,_) -> 0
                              (True ,_) -> 1
                    in do rest <- countM verbose xs
                          return (rest + res)
                where
                  handler :: SomeException -> IO (Bool, String)
                  handler err = return (False, "Crashed: ")

{- | Return True only if we find "-v" among the command-line arguments.  If the command
     line is empty, then return False.  If the commmand-line is not empty and doesn't
     contain -v, then print a synopsis and exit.
-}
interpretArgs :: [String] -> IO Bool
interpretArgs args | elem "-v" args   = do return True
                   | length args /= 0 = do printSynopsis; exitSuccess; return False
                   | True             = return False

{- | Print a synopsis of the program.
-}
printSynopsis :: IO ()
printSynopsis = do
    name <- getProgName
    putStrLn "\nUsage:"
    putStrLn $ "  " ++ name ++ " [-v]\n"
    putStrLn "-v:    Verbose.  Print out the passed test cases as well as the failed cases\n"
    putStrLn "Anything else on the command line causes this message to be printed and"
    putStrLn "the program will exit.\n"

{- | Acts a container for test cases. When this is evaluated, it returns a Bools result
     together with a text explanation.

     Parameters:
       1. Some arbitrary expression
       2. Some function that evaluates the expression against the expected value.
       3. The expected value that should result from the expression.
       4. A String representation of the expression (or reporting purposes)

     Returns a pair of Bool and String.  The Bool is True iff the evaluation of
     the expression matching the expected value by the function.  The String component
     is a text explanation of the result.
-}
test :: (Eq a, Show a, Ord a) => a -> (a -> a -> Bool) -> a -> String -> (Bool,String)
test exp f val str | f exp val = (True,  "Passed: " ++ str ++ " -- " ++ (show exp))
                   | True      = (False, "Failed: " ++ str ++ " -- " ++ (show exp) ++ " vs " ++ (show val))

{- | Reformat an explanation string conditionally: if Verbose is set and the is messing
     begins with "Passed", the suppress the message.  Otherwise, just return the message
     adding a linefeed.
-}
format :: Bool -> String -> String
format False msg | (take 6 msg) == "Passed" = ""
format _     msg                            = msg ++ "\n"

{- | A default grading scheme using a linear projection beween 50% to 100% for D to A.
-}
computeGrade :: Float -> Float -> String
computeGrade mark outof = normalizeGP $ 4 * mark / outof

{- | Conversion from a floating point in the range 0:4.3 to letter grades.
-}
normalizeGP :: Float -> String
normalizeGP gp
          | gp >= 4.15 = "A+"
          | gp >= 3.85 = "A"
          | gp >= 3.5  = "A-"
          | gp >= 3.15 = "B+"
          | gp >= 2.85 = "B"
          | gp >= 2.5  = "B-"
          | gp >= 2.15 = "C+"
          | gp >= 1.85 = "C"
          | gp >= 1.5  = "C-"
          | gp >= 1.15 = "D+"
          | gp >= 0.85 = "D"
          | True       = "F"

-- count :: Eq a => a -> [a] -> Int
-- count x [] = 0
-- count x (y:ys) = (if x == y then 1 else 0) + count x ys

{- | Compare two "sets" with paying attention to the order of the two elements.
-}
compareSets :: Ord a => [a] -> [a] -> Bool
compareSets a b = sort a == sort b

{- Compare "Either String Bool" objects, ignoring the actual value of Left constructor.
-}
compareEither :: Either String Bool -> Either String Bool -> Bool
compareEither (Right x) (Right y) = x == y
compareEither (Right _) (Left _)  = False
compareEither (Left _)  (Right _)  = False
compareEither (Left s)  (Left t) = True -- detail this

{- Compare Maybe [a] objects using the compareSets() funciton.
-}
compareMaybe :: Ord a => Maybe [a] -> Maybe [a] -> Bool
compareMaybe Nothing  Nothing  = True
compareMaybe Nothing  (Just _) = False
compareMaybe (Just _) Nothing  = False
compareMaybe (Just s) (Just t) = compareSets s t