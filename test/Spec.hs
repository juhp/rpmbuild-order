import Test.Hspec
import Control.Monad.Extra
--import Distribution.RPM.Build.Graph
import Distribution.RPM.Build.Order
import System.Posix.Files

main :: IO ()
main = setupSymlinks >> hspec spec

pkg :: FilePath -> FilePath
pkg = ("test/pkgs/" ++)

spec :: Spec
spec = do
  describe "sorting" $ do
    it "sort A B" $
      dependencySort [pkg "A", pkg "B"] >>=
      (`shouldBe` [pkg "B", pkg "A"])

    it "sort 1 2" $
      dependencySort [pkg "1", pkg "2"] >>=
      (`shouldBe` [pkg "2", pkg "1"])

    it "sort A/ B/" $
      dependencySort [pkg "A/", pkg "B/"] >>=
      (`shouldBe` [pkg "B/", pkg "A/"])

    it "sort 1/ 2/" $
      dependencySort [pkg "1/", pkg "2/"] >>=
      (`shouldBe` [pkg "2/", pkg "1/"])

    it "sort A.spec B.spec" $
      dependencySort [pkg "A/A.spec", pkg "B/B.spec"] >>=
      (`shouldBe` [pkg "B/B.spec", pkg "A/A.spec"])

    it "sort 1.spec 2.spec" $
      dependencySort [pkg "1/A.spec", pkg "2/B.spec"] >>=
      (`shouldBe` [pkg "2/B.spec", pkg "1/A.spec"])

    it "sort A/ B.spec" $
      dependencySort [pkg "A/", pkg "B/B.spec"] >>=
      (`shouldBe` [pkg "B/B.spec", pkg "A/"])

    it "circular A B C" $
      dependencySort [pkg "A", pkg "B", pkg "C"]
      `shouldThrow` anyException

    it "sort A B D1.0" $
      dependencySort [pkg "A", pkg "B/", pkg "D1.0"] >>=
      (`shouldBe` [pkg "B/", pkg "D1.0", pkg "A"])

    it "circular A B C boot" $
      dependencySortRpmOpts ["--with=boot"] [pkg "A", pkg "B", pkg "C"] >>=
      (`shouldBe` [pkg "C", pkg "B", pkg "A"])

  describe "layers" $
    it "layers A B" $
      dependencyLayers [pkg "A", pkg "B"] >>=
      (`shouldBe` [[pkg "B"], [pkg "A"]])

  describe "leaves" $
    it "leaves A B D" $
      leafPackages [pkg "A", pkg "B", pkg "D1.0"] >>=
      (`shouldBe` [pkg "A", pkg "D1.0"])

setupSymlinks :: IO ()
setupSymlinks =
  forM_ [("1","A"),("2","B")] $ \ (l,f) ->
    unlessM (fileExist $ pkg l) $
    createSymbolicLink f (pkg l)
