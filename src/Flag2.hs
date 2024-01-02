module Flag2 where

import           Data.Maybe
import           System.Console.GetOpt

data Flag = Flag { getName        :: String
                 , getValue       :: String
                 , getDescription :: String
                 }

--data Flag = Verbose | Version | Input String | Output String | LibDir String deriving Show

options :: [OptDescr Flag]
options = [ Option [] ["http-host"] (ReqArg input "huy") "http host"
          , Option [] ["http-port"] (ReqArg input "port") "http port"
          ]

input :: String -> Flag
input x = Input x

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv =
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (o,n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: ic [OPTION...] files..."

-- data Options = Options
--                { optVerbose     :: Bool
--                , optShowVersion :: Bool
--                , optOutput      :: Maybe FilePath
--                , optInput       :: Maybe FilePath
--                , optLibDirs     :: [FilePath]
--                } deriving Show

-- defaultOptions    = Options
--                     { optVerbose     = True
--                     , optShowVersion = False
--                     , optOutput      = Nothing
--                     , optInput       = Nothing
--                     , optLibDirs     = []
--                     }

-- options :: [OptDescr (Options -> Options)]
-- options =
--   [ Option ['v']     ["verbose"]
--     (NoArg (\ opts -> opts { optVerbose = True }))
--     "chatty output on stderr"
--   , Option ['V','?'] ["version"]
--     (NoArg (\ opts -> opts { optShowVersion = True }))
--     "show version number"
--   , Option ['o']     ["output"]
--     (OptArg ((\ f opts -> opts { optOutput = Just f }) . fromMaybe "output")
--       "FILE")
--     "output FILE"
--   , Option ['c']     []
--     (OptArg ((\ f opts -> opts { optInput = Just f }) . fromMaybe "input")
--       "FILE")
--     "input FILE"
--   , Option ['L']     ["libdir"]
--     (ReqArg (\ d opts -> opts { optLibDirs = optLibDirs opts ++ [d] }) "DIR")
--     "library directory"
--   ]

-- compilerOpts :: [String] -> IO (Options, [String])
-- compilerOpts argv =
--   case getOpt Permute options argv of
--     (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
--     (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
--   where header = "Usage: ic [OPTION...] files..."
