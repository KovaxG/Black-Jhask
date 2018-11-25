{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  putStrLn "Starting server on "
  putStrLn "http://localhost:8080/"
  run 8080 app

app :: Application
app request respond = do
  putStrLn "I've done some IO here"
  putStrLn . show $ request
  respond $ responseLBS
      status200
      [("Content-Type", "text/plain")]
      "Hello, Web!"    
