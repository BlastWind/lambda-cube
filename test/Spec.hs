import Test.Hspec
import Untyped.Spec (untypedSpec)

main :: IO ()
main = hspec spec

spec :: Spec 
spec = describe "Combined" $ do
    untypedSpec



