import           Test.Hspec
import           Parser

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "parseToC" $ do
        it "Simple on line" $
            Parser.parseToC "camelCase\n" `shouldBe` "camel_case\n"
        it "Multiple in one line" $
            Parser.parseToC "camelCase andCamel\n" `shouldBe` "camel_case and_camel\n"   
        it "Multiple in one line with _" $
            Parser.parseToC "camelCase and_Camel\n" `shouldBe` "camel_case and_Camel\n"  
        it "Complex in one line with _" $
            Parser.parseToC "came_lC_Ase anDCamel\n" `shouldBe` "came_l_c_Ase an_dCamel\n"  
        it "Simple one line with spaces" $
            Parser.parseToC "camelCase Camel case\n" `shouldBe` "camel_case Camel case\n" 
        it "Simple two lines" $
            Parser.parseToC "camelCase Camelcase\n camelCase\n" `shouldBe` "camel_case Camelcase\n camel_case\n"                   

