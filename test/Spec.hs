{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Monad ((<=<))
import           Lib
import           Test.Hspec

main = hspec spec

runFile :: FilePath -> IO (Either Err [Char])
runFile file = (fmap extractOutput . runProgram <=< readInput) <$> readFile file

spec =
  describe "editor" $ do
    it "operates correctly on given test cases" $ do
      runFile "fixtures/sample.txt" `shouldReturn` Right "cya"
