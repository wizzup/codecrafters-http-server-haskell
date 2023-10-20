{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Char8 as BS
import Network.Simple.TCP

main :: IO ()
main = do
    -- You can use print statements as follows for debugging, they'll be visible when running tests.
    BS.putStrLn "Logs from your program will appear here"

    -- Uncomment this block to pass first stage
    let host = "127.0.0.1"
        port = "4221"
    
    BS.putStrLn $ "Listening on " <> BS.pack host <> ":" <> BS.pack port
    
    serve (Host host) port $ \(serverSocket, serverAddr) -> do
         BS.putStrLn $ "Accepted connection from " <> BS.pack (show serverAddr) <> "."

         rcv <- fmap (BS.take 6 . head . BS.lines) <$> recv serverSocket 4096
         case rcv of
            Just "GET / " -> do
                BS.putStrLn "OK"
                send serverSocket "HTTP/1.1 200 OK\r\n\r\n"
            Just _  -> do
                BS.putStrLn "Not Found"
                send serverSocket "HTTP/1.1 404 Not Found\r\n\r\n"
            Nothing -> BS.putStrLn "Nothing"


        
