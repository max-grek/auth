module Flag.Internal where

import           Control.Monad              (join)
import           Data.Functor.Contravariant (Predicate (..))
import           Data.Functor.Identity
import           Data.List                  (group, intercalate, isPrefixOf,
                                             sort)
import           Data.List.Split            (splitOn)
import           Data.Maybe                 (fromMaybe)
import           System.Environment         (getArgs)

data Flag = Flag { getName          :: !String
                   , getValue       :: !String
                   , getDescription :: !String
                   } deriving Show

define :: Show a => String -> a -> String -> Flag
define name value = Flag name (show value)

data Error = UnknownFlags String | FlagSyntax String deriving Show

type Flags = [Flag]
type Args = [String]
type FlagNames = [String]

_countOcc :: Eq a => a -> [a] -> Int
_countOcc x xs = length $ filter (== x) xs

_splitBy :: Eq a => a -> [a] -> Maybe ([a],[a])
_splitBy x xs = case splitOn [x] xs of
              (x:y:_) -> Just (x, y)
              (x:_)   -> Nothing

_forSyntax :: [Predicate String] -> Args -> Either Error ()
_forSyntax ps args = go . join $ filter . getPredicate <$> ps <*> [args]
  where
    go :: [String] -> Either Error ()
    go xs
      | not $ null xs = Left $ FlagSyntax $ intercalate ", " xs
      | otherwise = return ()

_forUnknown :: FlagNames -> Args -> Either Error ()
_forUnknown flags args
  | not $ null unknown_flags = Left $ UnknownFlags $ intercalate ", " unknown_flags
  | otherwise = return ()
  where
    unknown_flags :: [String]
    unknown_flags = foldl (flip filter) args prefixed_flags

    prefixed_flags :: [String -> Bool]
    prefixed_flags = (not .) . isPrefixOf <$> flags -- not . (isPrefixOf "a")

_check :: FlagNames -> Args -> Either Error ()
_check flags args = do
  _forSyntax syntax_predicates args
  _forUnknown flags $ map (drop 2) args
  where
    syntax_predicates = [correct_dashes, one_eq_sign]

    correct_dashes :: Predicate String
    correct_dashes = Predicate $ not . isPrefixOf "--"

    one_eq_sign :: Predicate String
    one_eq_sign = Predicate $ (/= 1) . _countOcc '='

type FlagNameValuePairs = [(String,String)]
type ArgNameValuePairs = [(String,String)]

parse :: Flags -> Args -> Either Error [(String,String)]
parse flags args = do
  _check (sort $ map getName flags) (map head $ group $ sort args)
  pure $ merge name_value_pair $ map (fromMaybe ("","") . _splitBy '=' . drop 2) args
  where
    name_value_pair = zip (map getName flags) (map getValue flags)

    merge :: FlagNameValuePairs -> ArgNameValuePairs -> FlagNameValuePairs
    merge [] args = []
    merge (x:xs) args =
      case lookup (fst x) args of
        Just v  -> (fst x, v) : merge xs args
        Nothing -> x : merge xs args

