import Test.Hspec
--import Distribution.RPM.Build.Graph
import Distribution.RPM.Build.Order

main :: IO ()
main = hspec spec

pkg :: FilePath -> FilePath
pkg = ("test/pkgs/" ++)

spec :: Spec
spec = do
  describe "sorting" $ do
    it "sort A B" $
      dependencySort [pkg "A", pkg "B"] >>=
      (`shouldBe` [pkg "B", pkg "A"])

    it "sort A/ B/" $
      dependencySort [pkg "A/", pkg "B/"] >>=
      (`shouldBe` [pkg "B/", pkg "A/"])

    it "sort A.spec B.spec" $
      dependencySort [pkg "A/A.spec", pkg "B/B.spec"] >>=
      (`shouldBe` [pkg "B/B.spec", pkg "A/A.spec"])

    it "sort A/ B.spec" $
      dependencySort [pkg "A/", pkg "B/B.spec"] >>=
      (`shouldBe` [pkg "B/B.spec", pkg "A/"])

    it "circular A B C" $
      dependencySort [pkg "A", pkg "B", pkg "C"]
      `shouldThrow` anyException

    it "sort A B D1.0" $
      dependencySort [pkg "A", pkg "B/", pkg "D1.0"] >>=
      (`shouldBe` [pkg "B/", pkg "D1.0", pkg "A"])

  describe "layers" $
    it "layers A B" $
      dependencyLayers [pkg "A", pkg "B"] >>=
      (`shouldBe` [[pkg "B"], [pkg "A"]])

  describe "leaves" $
    it "leaves A B D" $
      leafPackages [pkg "A", pkg "B", pkg "D1.0"] >>=
      (`shouldBe` [pkg "A", pkg "D1.0"])
