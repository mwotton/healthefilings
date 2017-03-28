{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
import           Control.Monad         ((<=<))
import           Data.DeriveTH
import           Data.Monoid
import           Lib
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck       hiding (output)

derive makeArbitrary ''Operation


runFile :: FilePath -> IO (Either Err [Char])
runFile file = (fmap extractOutput . runProgram <=< readInput) <$> readFile file


main = hspec spec
spec =
  describe "editor" $ do
    it "operates correctly on given test cases" $ do
      runFile "fixtures/sample.txt" `shouldReturn` Right "cya"

    modifyMaxSuccess (const 1000) $
      prop "allows arbitrary insertion & deletion" $
      \(before::[Operation],s::String) ->
        successfulProgramsOnly before (before <> [Append s, Delete (fromIntegral $ length s)]) equivModuloHistory

    modifyMaxSuccess (const 1000) $
      prop "can undo anything but a print or an undo" $
      \(ops::[Operation],extraOp) ->
        (case extraOp of
            Append _ -> True
            Delete _ -> True
            _        -> False) ==>
        successfulProgramsOnly (ops <> [extraOp,Undo]) ops equivModuloHistory

successfulProgramsOnly p1 p2 equivalency =
  case (runProgram p1, runProgram p2) of
    (Right s1, Right s2) -> equivalency s1 s2
    -- this is a bit grotty, but unsure how to pass.
    _ -> 1 === 1

equivModuloHistory s1 s2 = (current s1, output s1) === (current s2, output s2)
