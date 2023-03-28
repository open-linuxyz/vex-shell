import Test.Hspec
import Lib
import Types

main :: IO ()
main = hspec $ do
  describe "parseEnvVars" $ do
    it "parses a string of environment variables into a list of key-value pairs" $ do
      let envVars = "VAR1=value1\nVAR2=value2\n"
      parseEnvVars envVars `shouldBe` [("VAR1", "value1"), ("VAR2", "value2")]

  describe "chooseEnvironment" $ do
    it "prompts the user to choose an environment" $ do
      let envs = [ Environment "env1" ""
                 , Environment "env2" ""
                 , Environment "env3" ""
                 ]
      -- TODO: test user input
      chooseEnvironment envs

  describe "listEnvironments" $ do
    it "lists all available environments" $ do
      -- TODO: create test environments and verify they are listed
      listEnvironments `shouldReturn` []
