{-# LANGUAGE OverloadedStrings #-}
 
import Text.Printf
import System.Environment( getArgs )
import System.Exit
import System.Console.GetOpt

import Network.HTTP
import Network.Socket
import Network.HTTP.Conduit
import Network.HTTP.Types

import qualified Data.ByteString.Lazy as L
import qualified Codec.Binary.UTF8.String as S
import qualified Data.ByteString.Lazy.Char8 as BS

import Data.IORef
import Data.List

import Data.Aeson
import Data.Aeson.Types hiding ( Options, defaultOptions )

import Control.Applicative

version :: String
version = "0.0.1"

main = do args <- getArgs
          let ( actions, nonOpts, msgs ) = getOpt RequireOrder options args
          Options { optstuff = dostuff 
                  } <- foldl (>>=) (return defaultOptions) actions
          dostuff

data Options = Options  { optstuff :: IO() }

makeRequest :: String -> IO String
makeRequest rq = withSocketsDo
    $ let url = "http://keybase.io/" ++ rq
      in simpleHttp url >>= \bs -> return ( S.decode $ L.unpack bs )

username :: [String] -> IO String
username usernames =
    let cusernames = intercalate "," usernames
    in makeRequest $ "_/api/1.0/user/lookup.json?usernames=" ++ cusernames

impera :: IO()
impera = do
    test <- newIORef 1 -- I'm so imperative
    
    x <- readIORef test
    writeIORef test $ x + 1
    putStrLn $ "* Test #" ++ (show x)
    
    putStrLn "getting info about Lena..."
    u <- username [ "lena" ]
    putStrLn "decoding JSON..."
    let Just pu = do result <- decode (BS.pack u)
                     flip parseMaybe result $ \obj -> do
                         them       <- obj          .: "them"
                         pictures   <- (them !! 0)  .: "pictures"
                         primary    <- pictures     .: "primary"
                         url        <- primary      .: "url"
                         return (url :: String)
    putStrLn $ ("Lena picture url: " ++ pu)
    
    x <- readIORef test
    writeIORef test $ x + 1
    putStrLn $ "* Test #" ++ (show x)
    
    let Just kf = do result <- decode (BS.pack u)
                     flip parseMaybe result $ \obj -> do
                         them               <- obj          .: "them"
                         public_keys        <- (them !! 0)  .: "public_keys"
                         primary            <- public_keys  .: "primary"
                         key_fingerprint    <- primary      .: "key_fingerprint"
                         return (key_fingerprint :: String)
    putStrLn $ ("Lena key_fingerprint: " ++ kf)

defaultOptions :: Options
defaultOptions = Options {
    optstuff = impera
    }
options :: [OptDescr (Options -> IO Options)]
options = [
    Option ['v'] ["version"] (NoArg showVersion) "show Tyapa version number",
    Option ['h'] ["help"]    (NoArg showHelp) "display this help"
    ]
showVersion _ = printf "\n  Keybase v.%s\n\n" version
                    >> exitWith ExitSuccess
showHelp _ = do putStrLn $ usageInfo "Usage: Keybase [optional things]" options
                exitWith ExitSuccess
