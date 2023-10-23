{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L
import Data.Maybe
import Network.Simple.TCP
import Options.Applicative
import System.Directory
import System.FilePath

data Cmd = NOP | GET
  deriving (Eq, Show)

data URI
  = UErr
  | URoot
  | UEcho BS.ByteString
  | UAgent
  | UFile FilePath
  deriving (Eq, Show)

data Ver = VErr | V1_1 deriving (Show)

data Request = Request
  { rCmd :: Cmd,
    rUri :: URI,
    rVer :: Ver
  }
  deriving (Show)

parseCmd :: BS.ByteString -> Cmd
parseCmd "GET" = GET
parseCmd _ = NOP

parseURI :: BS.ByteString -> URI
parseURI "/" = URoot
parseURI "/user-agent" = UAgent
parseURI s =
  fromMaybe UErr $
    f UEcho "/echo/"
      <|> f (UFile . BS.unpack) "/files/"
  where
    f c p = c <$> BS.stripPrefix p s

parseVer :: BS.ByteString -> Ver
parseVer "HTTP/1.1" = V1_1
parseVer _ = VErr

parse :: BS.ByteString -> Request
parse xs = case BS.words xs of
  [c, u, v] -> Request (parseCmd c) (parseURI u) (parseVer v)
  _ -> undefined

parseAgent :: [BS.ByteString] -> BS.ByteString
parseAgent s =
  BS.dropEnd 1
    $ BS.takeWhileEnd (/= ' ')
      . fromMaybe "  "
    $ L.find (BS.isPrefixOf "User-Agent:") s

workDirParser :: Parser (Maybe FilePath)
workDirParser = optional $ strOption $ long "directory"

repOk, repOkF, rep404 :: BS.ByteString -> BS.ByteString
repOk = rep "200 OK" "text/plain"
repOkF = rep "200 OK" "application/octet-stream"
rep404 = rep "404 Not Found" "text/plain"

rep :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
rep s t r =
  "HTTP/1.1 "
    <> s
    <> "\r\n"
    <> "Content-Type: "
    <> t
    <> "\r\n"
    <> "Content-Length: "
    <> BS.pack (show $ BS.length r)
    <> "\r\n\r\n"
    <> r

main :: IO ()
main = do
  workdir <- execParser $ info workDirParser (progDesc "Web server")
  putStrLn $ "Watching directory " <> fromMaybe "<None>" workdir

  let host = "127.0.0.1"
      port = "4221"
  putStrLn $ "Listening on " <> host <> ":" <> port

  serve (Host host) port $ \(serverSocket, serverAddr) -> do
    putStrLn $ "Accepted connection from " <> show serverAddr <> "."
    rcv <- fmap BS.lines <$> recv serverSocket 4096
    mapM_ (mapM_ print) rcv
    case rcv of
      Nothing -> putStrLn "Ignored Nothing"
      Just req -> do
        case rUri $ parse $ head req of
          URoot -> do
            putStrLn "Root"
            send serverSocket (repOk BS.empty)
          UEcho e -> do
            putStrLn $ "Echo: " <> BS.unpack e
            send serverSocket (repOk e)
          UAgent -> do
            putStrLn "User Agent"
            let agent = parseAgent $ tail req
            send serverSocket (repOk agent)
          UFile f -> do
            putStrLn "Files"
            case workdir of
              Nothing -> send serverSocket (rep404 "no workdir")
              Just dir -> do
                let file = dir </> f
                putStrLn $ "File" <> file
                doesFileExist file >>= \case
                  True -> do
                    putStrLn "File Exists"
                    ctx <- readFile file
                    send serverSocket (repOkF $ BS.pack ctx)
                  False -> do
                    putStrLn "File Not Exists"
                    send serverSocket (rep404 $ BS.pack (show dir) <> "/" <> BS.pack f <> " Not Exists")
          UErr -> do
            putStrLn "Non-echo"
            send serverSocket (rep404 "Non-echo")
