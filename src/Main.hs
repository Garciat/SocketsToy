module Main where

import Data.List
import Control.Monad
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import System.Environment
import System.Timeout
import Control.Monad
import Control.Exception
import Control.Concurrent
import Network.Socket
import qualified Network.Socket.ByteString as BS
import Network.URI
import Network.Mime
import qualified Filesystem as FS
import qualified Filesystem.Path as Path
import qualified Filesystem.Path.CurrentOS as Path

ignore :: Monad m => m ()
ignore = return ()

ofShow :: Show a => a -> Parser a
ofShow x = do
  string (show x)
  return x

newtype Uri = Uri String deriving (Show, Eq)

uri :: Parser URI
uri = do
  s <- many1 (noneOf " ")
  case parseRelativeReference s of
    Nothing  -> unexpected "Invalid URI."
    Just uri -> return uri

data StdHttpMethod = OPTIONS
                   | GET
                   | HEAD
                   | POST
                   | PUT
                   | DELETE
                   | TRACE
                   | CONNECT
                   deriving (Show, Eq, Enum)

data HttpMethod = StandardMethod StdHttpMethod
                | GenericMethod String
                deriving (Show, Eq)

standardMethod :: Parser HttpMethod
standardMethod = do
  m <- choice methods
  return (StandardMethod m)
  where
    methods = map ofShow [OPTIONS .. CONNECT]

genericMethod :: Parser HttpMethod
genericMethod = do
  m <- many1 (oneOf ['A'..'Z'])
  return (GenericMethod m)

method :: Parser HttpMethod
method = standardMethod <|> genericMethod

data HttpHeader = GenericHttpHeader String String deriving (Show, Eq)

headerKeyChars :: [Char]
headerKeyChars = ['\32'..'\127'] \\ "()<>@,;:\\\"/[]?={} "

endl :: Parser ()
endl = do
  optional (char '\r')
  char '\n' >> ignore

genericHeader :: Parser HttpHeader
genericHeader = do
  k <- many1 (oneOf headerKeyChars)
  string ": "
  v <- many1 (noneOf "\r\n")
  endl
  let h = GenericHttpHeader k v
  return h

headers :: Parser [HttpHeader]
headers = many genericHeader

fromHeader :: HttpHeader -> String
fromHeader (GenericHttpHeader k v) = k ++ ": " ++ v ++ "\r\n"

fromHeaders :: [HttpHeader] -> String
fromHeaders hs = concat $ map fromHeader hs

data HttpRequest = HttpRequest { reqMethod :: HttpMethod
                               , reqURI :: URI
                               , reqVersion :: String
                               , reqHeaders :: [HttpHeader]
                               } deriving (Show)

version :: Parser String
version = string "HTTP/1.1"

request :: Parser HttpRequest
request = do
  m <- method
  space
  u <- uri
  space
  v <- version
  endl
  hs <- headers
  endl
  let req = HttpRequest m u v hs
  return req

data StatusCode = StatusCode Int String deriving (Show, Eq)

fromStatusCode :: StatusCode -> String
fromStatusCode (StatusCode i s) = show i ++ " " ++ s

data HttpResponse = HttpResponse { resVersion :: String
                                 , resStatus :: StatusCode
                                 , resHeaders :: [HttpHeader]
                                 , resBody :: Maybe BS.ByteString
                                 } deriving (Show)

sendResponse :: HttpResponse -> Socket -> IO ()
sendResponse res sock = do
  BS.sendAll sock (BS8.pack responseHead)
  case resBody res of
    Nothing   -> ignore
    Just body -> BS.sendAll sock body
  where
    responseHead = resVersion res ++ " "
                    ++ fromStatusCode (resStatus res) ++ "\r\n"
                    ++ fromHeaders (resHeaders res) ++ "\r\n"

server sock addr = do
  bind sock (addrAddress addr)
  listen sock sOMAXCONN
  tid <- forkOS $ forever $ do
    (client, clientAddr) <- accept sock
    putStrLn $ ("Client: " ++ show clientAddr)
    msgM <- timeout 5000000 (recv client 4096)
    handleMessage client msgM
    close client
  _ <- getLine
  killThread tid
  putStrLn "Bye."
  where
    handleMessage _ Nothing   = putStrLn "Request timeout."
    handleMessage _ (Just "") = putStrLn "Client left."
    
    handleMessage client (Just msg) = do
      case (parse request "stdin" msg) of
        Left err  -> sendBadRequest client (show err)
        Right req -> catch (sendOk client req) (handleException client req)
    
    defaultHeaders = [GenericHttpHeader "Connection" "close"]
    
    sendBadRequest client err = do
      let res = HttpResponse "HTTP/1.1" (StatusCode 400 "Bad Request") defaultHeaders (Just $ BS8.pack err)
      sendResponse res client
    
    handleException client req (SomeException e) = do
      let err = show e
      let res = HttpResponse "HTTP/1.1" (StatusCode 500 "Internal Server Error") defaultHeaders (Just $ BS8.pack err)
      sendResponse res client
    
    sendOk client req = do
      let path = Path.decodeString $ uriPath $ reqURI req
      isDir <- FS.isDirectory path
      if isDir then
        sendDirList client path
      else
        sendFile client path
    
    sendDirList client path = do
      entries <- FS.listDirectory path
      let body = BS8.pack $ unlines $ map Path.encodeString entries
      let hs = defaultHeaders
                ++ [GenericHttpHeader "Content-Type" "text/plain"]
                ++ [GenericHttpHeader "Content-Length" (show $ BS.length body)]
      let res = HttpResponse "HTTP/1.1" (StatusCode 200 "OK") hs (Just body)
      sendResponse res client
    
    sendFile client path = do
      bytes <- FS.readFile path
      let body = bytes
      let mime = T.unpack $ T.decodeUtf8 $ defaultMimeLookup (T.pack $ Path.encodeString path)
      let hs = defaultHeaders
                ++ [GenericHttpHeader "Content-Type" mime]
                ++ [GenericHttpHeader "Content-Length" (show $ BS.length body)]
      let res = HttpResponse "HTTP/1.1" (StatusCode 200 "OK") hs (Just bytes)
      sendResponse res client

main = do
  [addr, portS] <- getArgs
  let hints = defaultHints { addrFlags = [AI_ADDRCONFIG, AI_CANONNAME] }
  addrs <- getAddrInfo (Just hints) (Just addr) (Just portS)
  let addr = head addrs
  putStrLn $ ("Addr: " ++ show addr)
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  server sock addr
  close sock