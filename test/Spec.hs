import           Test.Hspec
import           Parser
import           System.IO.Error                ( isDoesNotExistError )
import           System.IO
import           System.Directory
import           System.FilePath

main :: IO ()
main = hspec spec

-- cTestFileConent :: IO Bool
-- cTestFileConent = 
--     (readFile "test/testFolder/test.c") == return "#include \"game.h\n\n\"int initializeSdl(void) {\n}"

spec :: Spec
spec = do
    describe "start" $ do
        it "No such directory"
            $             Parser.start "-c" "test/testFolder1"
            `shouldThrow` isDoesNotExistError
        -- it "Parse on file to OO style"
        --     $             Parser.start "-c" "test/testFolder/test.c"
        --     `shouldSatisfy` readFile "test/testFolder/test.c" == return "#include \"game.h\n\n\"int initializeSdl(void) {\n}"  

    describe "parseToC" $ do
        it "Simple one line"
            $          Parser.parseToC "camelCase\n"
            `shouldBe` "camel_case\n"
        it "Multiple in one line"
            $          Parser.parseToC "camelCAse andCamel\n"
            `shouldBe` "camel_cAse and_camel\n"
        it "Multiple in one line with _"
            $          Parser.parseToC "camelCase and_Camel\n"
            `shouldBe` "camel_case and_Camel\n"
        it "Complex in one line with _"
            $          Parser.parseToC "came_lC_Ase anDCamel\n"
            `shouldBe` "came_l_c_Ase an_dCamel\n"
        it "Simple one line with spaces"
            $          Parser.parseToC "camelCase Camel case\n"
            `shouldBe` "camel_case Camel case\n"
        it "Simple two lines"
            $          Parser.parseToC "camelCase Camelcase\n camelCase\n"
            `shouldBe` "camel_case Camelcase\n camel_case\n"

    describe "parseToOO" $ do
        it "Simple one line"
            $          Parser.parseToOO "camel_case\n"
            `shouldBe` "camelCase\n"
        it "Multiple in a sequence one line"
            $          Parser.parseToOO "camel_c_a_se\n"
            `shouldBe` "camelCASe\n"
        it "One line with upper case after underscore"
            $          Parser.parseToOO "camel_Case\n"
            `shouldBe` "camel_Case\n"
        it "Multiple in one line"
            $          Parser.parseToOO "camel_c_ase and_camel\n"
            `shouldBe` "camelCAse andCamel\n"
        it "Multiple in one line with double underscore"
            $          Parser.parseToOO "camel_case and__Camel\n"
            `shouldBe` "camelCase and__Camel\n"
