module Main where

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["gpg"] -> showGpg
    ["gnupg"] -> showGpg
    ["gpt"] -> decryptGpt
    ["openai-gpt"] -> decryptGpt
    ["help"] -> showHelp
    ["h"] -> showHelp
    ["-h"] -> showHelp
    ["--help"] -> showHelp
    ["homedir"] -> showHomeDir
    ["home"] -> showHomeDir
    ["~"] -> showHomeDir
    ["hostname"] -> showHostname
    ["computer"] -> showHostname
    ["ssh"] -> showSsh
    ["ssh", argflag] -> showSshWithFlag argflag
    _ -> putStrLn "Invalid option. Use 'showme help' for more information."

showGpg :: IO ()
showGpg = putStrLn $ unlines
  [ "pub\tnistp521 2023-03-24 [S]"
  , "uid\tMichael Lloyd (mwl) <micl_dev@protonmail.com>"
  , "8A9126330BDC065C23A79C342F1E95D2CBB41C0C"
  ]

decryptGpt :: IO ()
decryptGpt = putStrLn "Decrypting GPT..."

showHelp :: IO ()
showHelp = putStrLn "Showing help information..."

showHomeDir :: IO ()
showHomeDir = putStrLn "Showing home directory information..."

showHostname :: IO ()
showHostname = putStrLn "Showing hostname information..."

showSsh :: IO ()
showSsh = showSshWithFlag ""

showSshWithFlag :: String -> IO ()
showSshWithFlag argflag = do
  let clipboardFlag = argflag `elem` ["-x", "--clipboard", "clipboard"]
      helpFlag = argflag `elem` ["-h", "help", "--help"]
  if helpFlag
    then putStrLn "Usage: showme ssh [-x|--clipboard|clipboard] [-h|--help|help]"
    else do
      keyFiles <- getKeyFiles
      let keyDescriptions = map extractKeyDescription keyFiles
      putStrLn $ unlines $ zipWith (\i desc -> show i ++ ") " ++ desc) [1..] keyDescriptions
      userInput <- prompt "Enter the number of the key you'd like to display: "
      case reads userInput of
        [(index, "")] -> do
          if index < 1 || index > length keyDescriptions
            then putStrLn "Invalid input. Please enter a valid number."
            else do
              clearScreen
              let keyFile = keyFiles !! (index - 1)
              if clipboardFlag
                then do
                  hasXclip <- hasCommand "xclip"
                  if hasXclip
                    then do
                      contents <- readFile keyFile
                      writeToClipboard contents
                      putStrLn "The selected public key has been copied to your clipboard."
                    else putStrLn "xclip is not installed. Please install it to use clipboard functionality."
                else do
                  contents <- readFile keyFile
                  putStrLn contents
        _ -> putStrLn "Invalid input. Please enter a valid number."

getKeyFiles :: IO [FilePath]
getKeyFiles = do
  let keyDir = "~/.ssh/public"
  keyFiles <- getDirectoryContents keyDir
  return [keyDir </> keyFile | keyFile <- keyFiles, isSuffixOf ".pub" keyFile]

extractKeyDescription :: FilePath -> String
extractKeyDescription keyFile
