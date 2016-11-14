{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, TemplateHaskell, FlexibleInstances #-}
----------------------------------------------------------------------
-- |
-- Module      :  Test.Simple
-- Copyright   :  (c) Boris Sukholitko 2012
-- License     :  BSD3
-- 
-- Maintainer  :  boriss@gmail.com
-- Stability   :  experimental
-- 
-- Test.Simple is yet another testing library for Haskell. It has testing primitives
-- familiar to recovering Perl programmers :).
-- 
-- Having 'MonadPlus' instance allows to cut tests early e.g using 'guard' function.
--
-- Being monad transformer it includes integration with 'QuickCheck' by declaring 'Testable' instance 
-- on @TestSimpleT Gen a@.
-- 
-- Test.Simple also has the ability to run in pure context (see 'runTestSimple' function).
--
-- Here is an example suitable for cabal test-suite integration. Note that TemplateHaskell
-- usage is optional and is needed for test failure locations only.
--
-- @
--{-\# LANGUAGE TemplateHaskell \#-}
--
--import Test.Simple
--import Control.Monad
--
--main :: IO ()
--main = testSimpleMain $ do
--          plan 7
--          ok True
--          is 1 1
--          isnt \"a\" \"b\"
--          like \"abcd\" \"bc\"
--          unlike \"a\" \"b\"
--          diag \"Successful so far, failures follow ...\"
--          $loc >> ok False \-\- location will be recorded
--          is \"a\" \"b\" >>= guard
--          diag \"I am not being called\" \-\- not reached because of the guard: MonadPlus FTW!
-- @
--
----------------------------------------------------------------------

module Test.Simple (
            -- * Types
            TestSimpleT, Likeable(isLike),
            
            -- * Main
            testSimpleMain, runTestSimple, qcTestSimpleWith, qcTestSimpleMain,
            
            -- * Plan
            plan,

            -- * Test functions
            ok, isnt, is, like, unlike, isRight,

            -- * Diagnostics
            loc, diag, diagen) where

import Control.Monad.Trans.State.Plus
import Control.Monad.State
import System.Exit (exitFailure)
import Data.List (isInfixOf, intercalate)
import System.IO (hPutStrLn, stderr)
import qualified Language.Haskell.TH as TH
import Test.QuickCheck (Testable(property), quickCheckResult, Gen)
import Test.QuickCheck.Test (isSuccess)
import qualified Test.QuickCheck.Test as Q
import Test.QuickCheck.Property (Result(reason), succeeded, failed)
import Test.QuickCheck.Monadic
import Control.Applicative

-- | Is used in 'like', 'unlike' tests.
class Likeable a b where
    -- | Returns 'True' if @a@ is like @b@
    isLike :: a -> b -> Bool

instance Eq a => Likeable [a] [a] where
    isLike = flip isInfixOf

data TSOutput = StdOut String | StdErr String
data TSState = TSS { tsCounter :: Int, tsFailed :: Int, tsPlanned :: Int, tsLoc :: Maybe TH.Loc
                            , tsOutput :: [TSOutput] }

-- | Test.Simple is implemented as monad transformer.
newtype TestSimpleT m a = MkTST { unTST :: StatePlusT TSState m a }
                                deriving (Functor, MonadTrans, Monad, MonadPlus
                                            , MonadState TSState, MonadIO, Alternative, Applicative)

emptyState :: TSState
emptyState = TSS 0 0 0 Nothing []

finishTS :: Monad m => TestSimpleT m a -> m (Bool, TSState)
finishTS m = do
    ms <- execStatePlusT (unTST m) emptyState
    finState <- execStatePlusT (unTST finish) ms
    return (not $ isFailed finState, finState)
    where
    finish = do
        s <- get
        let fld = tsFailed s > 0
        let mismatch = (tsPlanned s /= tsCounter s)
        if fld
                then diag $ "Looks like you failed " ++ show (tsFailed s)
                                ++ " test of " ++ show (tsPlanned s) ++ "."
                else if mismatch
                            then diag $ "Looks like you planned " ++ show (tsPlanned s)
                                                ++ " tests but ran " ++ show (tsCounter s) ++ "."
                            else return ()
        modify finOutput
        return $ not (fld || mismatch)
    finOutput s = s { tsOutput = (StdOut $ "1.." ++ show (tsPlanned s)):(reverse $ tsOutput s) }
    isFailed s = tsFailed s > 0 || (tsPlanned s /= tsCounter s)

-- | Runs 'TestSimpleT' transformer. Returns whether the tests where successful and resulting
-- output.
runTestSimple :: Monad m => TestSimpleT m a -> m (Bool, [String])
runTestSimple m = do
    (b, s) <- finishTS m
    return (b, map toStr $ tsOutput s)
    where toStr (StdOut str) = str
          toStr (StdErr str) = str

-- | Runs 'TestSimpleT' transformer in 'IO'. Outputs results in TAP format.
-- Exits with error on test failure.
--
testSimpleMain :: MonadIO m => TestSimpleT m a -> m ()
testSimpleMain m = do
    (b, s) <- finishTS m
    liftIO $ do
        mapM_ printLine $ tsOutput s
        unless b exitFailure
    where printLine (StdOut s) = putStrLn s
          printLine (StdErr s) = hPutStrLn stderr s

-- | Is @Bool@ ok?
ok :: Monad m => Bool -> TestSimpleT m Bool
ok b = do
    s <- get
    let oks = "ok " ++ show (tsCounter s + 1)
    put $ s { tsCounter = (tsCounter s) + 1
                , tsFailed = (tsFailed s) + if b then 0 else 1
                , tsOutput = (StdOut $ if b then oks else "not " ++ oks):(tsOutput s)
            }
    unless b $ diag $ concat [ "  Failed test", showLoc s ]
    return b

showLoc :: TSState -> String
showLoc s = " at " ++ go (tsLoc s) ++ "." where
    go Nothing = "unknown location"
    go (Just l) = concat [ TH.loc_filename l, " line ", show $ fst $ TH.loc_start l ]

(>>?) :: Monad m => m Bool -> m () -> m Bool
m >>? d = do
    b <- m
    unless b d
    return b

diagVals :: Monad m => String -> String -> String -> String-> TestSimpleT m ()
diagVals as a bs b = do
    diag $ concat [ spaces, as, " ", a ]
    diag $ concat [ bs, " ", b ]
    where spaces = take (length bs - length as) $ cycle " "

-- | Are values different?
isnt :: (Eq a, Show a, Monad m) => a -> a -> TestSimpleT m Bool
isnt a b = ok (a /= b) >>? diagVals "got:" (show a) "expected:" "anything else"

-- | Are values equal?
is :: (Eq a, Show a, Monad m) => a -> a -> TestSimpleT m Bool
is a b = ok (a == b) >>? diagVals "got:" (show a) "expected:" (show b)

-- | Is @a@ like @b@?
like :: (Show a, Show b, Likeable a b, Monad m) => a -> b -> TestSimpleT m Bool
like a b = ok (isLike a b) >>? diagVals "" (show a) "doesn't match" (show b)

-- | Is @a@ unlike @b@?
unlike :: (Show a, Show b, Likeable a b, Monad m) => a -> b -> TestSimpleT m Bool
unlike a b = ok (not $ isLike a b) >>? diagVals "" (show a) "matches" (show b)

-- | Is 'Either' right?
isRight :: (Monad m, Show a) => Either a b -> TestSimpleT m Bool
isRight (Right _) = ok True
isRight (Left a) = ok False >>? diagVals "got Left:" (show a) "expected:" "Right"

-- | Outputs diagnostics message.
diag :: Monad m => String -> TestSimpleT m ()
diag s = modify (\st -> st { tsOutput = (StdErr $ "# " ++ s):(tsOutput st) })

-- | Sets expected number of tests. Running more or less tests is considered failure.
-- Note, that plans are composable, e.g:
--
-- @
-- (plan 1 >> ok True) >> (plan 1 >> ok True)
-- @
--
-- will expect 2 tests.
plan :: Monad m => Int -> TestSimpleT m ()
plan i = modify (\st -> st { tsPlanned = tsPlanned st + i })

-- | Records current location to output in case of failures.
-- Necessary caveat: failing later without updating location produces the last location recorded.
loc :: TH.Q TH.Exp
loc = do
    l <- TH.location
    let ql = liftLoc l
    [| modify (\s -> s { tsLoc = Just $ql }) |]

liftLoc :: TH.Loc -> TH.Q TH.Exp
liftLoc l = [| TH.Loc f p m s e |] where
    f = TH.loc_filename l
    p = TH.loc_package l
    m = TH.loc_module l
    s = TH.loc_start l
    e = TH.loc_end l

instance Testable (TestSimpleT Gen a) where
    property m = property $ do
        (b, lns) <- runTestSimple m
        return $ if b then succeeded else failed { reason = intercalate "\n" lns }

instance Testable (TestSimpleT (PropertyM IO) a) where
    property m = monadicIO $ do
        (b, lns) <- runTestSimple m
        unless b $ run $ mapM_ putStrLn lns
        assert b

-- | Run some 'Testable' monad through 'QuickCheck' function. Exit with failure on error.
qcTestSimpleWith :: (m a -> IO Q.Result) -> m a -> IO ()
qcTestSimpleWith qc m = do
    res <- qc m
    unless (isSuccess res) exitFailure

-- | Run some 'Testable' monad through 'QuickCheck'. Exit with failure on error.
-- Equivalent to 'qcTestSimpleWith' 'quickCheckResult'
qcTestSimpleMain :: (Testable (m a)) => m a -> IO ()
qcTestSimpleMain = qcTestSimpleWith quickCheckResult

-- | Generates and logs (through 'diag') arbitrary value. Also outputs current location.
diagen :: Show a => String -> Gen a -> TestSimpleT Gen a
diagen msg gen = do
    a <- lift gen
    s <- get
    diag $ concat [ msg, ": ", show a, showLoc s ]
    return a
