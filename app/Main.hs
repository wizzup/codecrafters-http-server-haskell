{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Char8 as BS
import Network.Simple.TCP

main :: IO ()
main = do
    let host = "127.0.0.1"
        port = "4221"
    
    BS.putStrLn $ "Listening on " <> BS.pack host <> ":" <> BS.pack port
    
    serve (Host host) port $ \(serverSocket, serverAddr) -> do
        BS.putStrLn $ "Accepted connection from " <> BS.pack (show serverAddr) <> "."

        rcv <- fmap (BS.words . head . BS.lines) <$> recv serverSocket 4096
        print rcv
        case rcv of
            Just ("GET":"/":_) -> do
              BS.putStrLn "Root"
              send serverSocket "HTTP/1.1 200 OK\r\n\r\n"

            Just ("GET":p:_) -> do
                case BS.stripPrefix (BS.pack "/echo/") p of
                    Just x -> do
                        BS.putStrLn $ "Echo: " <> x
                        send serverSocket 
                            $ "HTTP/1.1 200 OK\r\n"
                            <> "Content-Type: text/plain\r\n"
                            <> "Content-Length: " <> (BS.pack . show $ BS.length x) <> "\r\n\r\n"
                            <> x <> "\r\n"
                    Nothing -> do
                        BS.putStrLn "Non-echo"
                        send serverSocket "HTTP/1.1 404 Not Found\r\n\r\n"

            _ -> BS.putStrLn "Ignored Nothing"
