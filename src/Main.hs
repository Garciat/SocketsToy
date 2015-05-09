module Main where

import Data.List
import Control.Monad
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

ignore = return ()

ofShow :: Show a => a -> Parser a
ofShow x = do
  string (show x)
  return x

newtype Uri = Uri String deriving (Show, Eq)

uri :: Parser Uri
uri = do
  s <- many1 (letter <|> digit <|> oneOf "/-._~")
  return (Uri s)

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
                               , reqUri :: Uri
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
                                 , resBody :: Maybe T.Text
                                 } deriving (Show)

fromResponse :: HttpResponse -> String
fromResponse res =
  resVersion res
  ++ " "
  ++ fromStatusCode (resStatus res)
  ++ "\r\n"
  ++ fromHeaders (resHeaders res)
  ++ "\r\n"
  ++ case resBody res of
        Nothing   -> ""
        Just body -> T.unpack body

main = do
  putStr $ fromResponse $ HttpResponse "HTTP/1.1" (StatusCode 200 "OK") [] Nothing
  input <- getContents
  case (parse request "stdin" input) of
    Left err  -> print err
    Right req -> print req
