module Lib
  ( runCommand
  ) where

import Control.Monad (when)
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import System.Environment (getArgs, getEnvironment, setEnv)
import System.Exit (exitFailure)
import System.IO (hFlush, stdout)
import System.Process (callCommand)
import Types

setenvDir :: IO FilePath
setenvDir = do
  homeDir <- getHomeDirectory
  return $ homeDir ++ "/.config/setenv"

loadEnvironment :: Environment -> IO ()
loadEnvironment env = do
  exists <- doesFileExist (envFile env)
  if exists
     then do
       envVars <- readFile (envFile env)
      mapM_ (uncurry setEnv) (parseEnvVars envVars)
      putStrLn $ "Environment '" ++ envName env ++ "' loaded."
    else do
      putStrLn $ "Error: Environment '" ++ envName env ++ "' not found."
      exitFailure

parseEnvVars :: String -> [(String, String)]
parseEnvVars = map ((\(k, v) -> (k, drop 1 v)) . break (=='=')) . lines

chooseEnvironment :: [Environment] -> IO ()
chooseEnvironment envs = do
  putStrLn "Choose an environment:"
  let envsWithNumbers = zip [1..] envs ++ [(length envs + 1, Environment "None" "")]
  mapM_ (\(n, env) -> putStrLn $ "  " ++ show n ++ ") " ++ envName env) envsWithNumbers
  putStr "Enter the number of your choice: "
  hFlush stdout
  choice <- getLine
  case reads choice of
    [(n, _)] | n
  then when (n > 0 && n <= length envs) $
    loadEnvironment (envs !! (n - 1))
    _ -> putStrLn "Invalid choice."

createEnvironment :: String -> IO ()
createEnvironment envName = do
  setenvDir <- setenvDir
let envFile = setenvDir ++ "/" ++ envName
exists <- doesFileExist envFile
if exists
then putStrLn $ "Error: Environment '" ++ envName ++ "' already exists."
else do
  envVars <- getEnvironment
writeFile envFile (unlines $ map ((k, v) -> k ++ "=" ++ v) envVars)
putStrLn $ "Environment '" ++ envName ++ "' created."

resetEnvironment :: IO ()
resetEnvironment = do
  envVars <- readFile (setenvDir ++ "/default.bak.env")
mapM_ (uncurry setEnv) (parseEnvVars envVars)
let clearEnvVar k = when (take 7 k == "SETENV_") $ setEnv k ""
mapM_ clearEnvVar =<< getEnvironment
putStrLn "Environment reset."

runCommand :: Command -> IO ()
runCommand (Load envName) = loadEnvironment $ Environment envName (setenvDir ++ "/" ++ envName)
runCommand Choose = do
  envs <- listEnvironments
chooseEnvironment envs
runCommand (New envName) = createEnvironment envName
runCommand Reset = resetEnvironment
runCommand Help = putStrLn "Usage:\n
                           \ setenv -n <ENVIRONMENT> | --new <ENVIRONMENT> Create a new environment and store current settings.\n
                           \ setenv -z Reset the current environment and unset SETENV_ variables.\n
                           \ setenv <ENVIRONMENT> Load the specified environment.\n
                           \ setenv Prompt the user to choose an environment from a list.\n
                           \ setenv -h | --help Show this help message."

listEnvironments :: IO [Environment]
listEnvironments = do
  setenvDir <- setenvDir
createDirectoryIfMissing True setenvDir
files <- map (setenvDir ++) <$> getDirectoryContents setenvDir
let envFiles = filter (\f -> length f > 2 && take 1 (takeFileName f) /= ".") files
let envNames = map (takeFileName) envFiles
return $ map (\n -> Environment n (setenvDir ++ "/" ++ n)) envNames
