{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Char8 as BS
import Network.Simple.TCP
import Data.List (find)
import Data.Maybe (fromMaybe)

data Cmd = NOP | GET
    deriving (Eq, Show)

data URI 
  = UErr
  | URoot
  | UEcho BS.ByteString
  | UAgent
  deriving (Eq, Show)

data Ver = VErr | V1_1 deriving (Show)

data Request = Request {
    rCmd :: Cmd,
    rUri :: URI,
    rVer :: Ver
} deriving (Show)

parseCmd :: BS.ByteString -> Cmd
parseCmd "GET" = GET
parseCmd _ = NOP

parseURI :: BS.ByteString -> URI
parseURI "/" = URoot
parseURI "/user-agent" = UAgent
parseURI s = maybe UErr UEcho (BS.stripPrefix "/echo/" s)

parseVer :: BS.ByteString -> Ver
parseVer "HTTP/1.1" = V1_1
parseVer _ = VErr

parse :: BS.ByteString -> Request
parse xs = case BS.words xs of
    [c,u,v] -> Request (parseCmd c) (parseURI u) (parseVer v)
    _ -> undefined

parseAgent :: [BS.ByteString] -> BS.ByteString
parseAgent s = BS.tail . BS.dropWhile (/= ' ') 
    . fromMaybe "  " $ find (BS.isPrefixOf "User-Agent:") s

main :: IO ()
main = do
    let host = "127.0.0.1"
        port = "4221"
    
    BS.putStrLn $ "Listening on " <> BS.pack host <> ":" <> BS.pack port
    
    serve (Host host) port $ \(serverSocket, serverAddr) -> do
        BS.putStrLn $ "Accepted connection from " <> BS.pack (show serverAddr) <> "."

        rcv <- fmap BS.lines <$> recv serverSocket 4096
        mapM_ (mapM_ print) rcv
        case rcv of
            Nothing -> BS.putStrLn "Ignored Nothing"
            Just req -> do
                case rUri $ parse $ head req of
                    URoot -> do
                      BS.putStrLn "Root"
                      send serverSocket "HTTP/1.1 200 OK\r\n\r\n"
                    UEcho x -> do
                      BS.putStrLn $ "Echo: " <> x
                      send serverSocket 
                          $ "HTTP/1.1 200 OK\r\n"
                          <> "Content-Type: text/plain\r\n"
                          <> "Content-Length: " <> (BS.pack . show $ BS.length x) <> "\r\n\r\n"
                          <> x
                    UAgent -> do
                      BS.putStrLn "User Agent"
                      let agent = parseAgent $ tail req
                      send serverSocket 
                          $ "HTTP/1.1 200 OK\r\n"
                          <> "Content-Type: text/plain\r\n"
                          <> "Content-Length: " <> (BS.pack . show $ BS.length agent - 1) <> "\r\n\r\n"
                          <> agent
                    UErr -> do
                      BS.putStrLn "Non-echo"
                      send serverSocket "HTTP/1.1 404 Not Found\r\n\r\n"
