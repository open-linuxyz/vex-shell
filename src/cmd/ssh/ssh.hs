import System.Directory
import System.Environment
import System.FilePath
import System.Process

loadSshKeys :: IO (Either String ())
loadSshKeys = do
  -- Directory containing your private SSH keys
  sshKeyDir <- getEnv "HOME"
  let sshKeyDirPath = sshKeyDir </> ".ssh" </> "private"

  -- Check if the directory exists
  dirExists <- doesDirectoryExist sshKeyDirPath
  if not dirExists
    then do
      return $ Left "directory does not exist"
    else do
      -- Iterate over each file in the directory and add it to the SSH agent
      dirContents <- listDirectory sshKeyDirPath
      let keyPaths = map (sshKeyDirPath </>) dirContents
      keyFiles <- filterM doesFileExist keyPaths
      results <- mapM addSshKey keyFiles
      if all (== ExitSuccess) results
        then do
          return $ Right ()
        else do
          return $ Left "Failed to add one or more SSH keys"

addSshKey :: FilePath -> IO ExitCode
addSshKey keyPath = do
  (_, _, _, ph) <- createProcess (proc "ssh-add" [keyPath]) { std_out = CreatePipe, std_err = CreatePipe }
  waitForProcess ph

main :: IO ()
main = do
  xdgRuntimeDir <- getEnv "XDG_RUNTIME_DIR"
  let sshAuthSock = xdgRuntimeDir </> "ssh-agent.socket"
  setEnv "SSH_AUTH_SOCK" sshAuthSock
  result <- loadSshKeys
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right _ -> putStrLn "SSH keys added successfully"
