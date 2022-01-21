module Parsers where
import Text.Megaparsec
  ( MonadParsec (try),
    Parsec,
    parse,
    some,
    (<|>),
    eof
  )
import Text.Megaparsec.Char (char', printChar, space, string')
import Text.Megaparsec.Error (ParseErrorBundle)
import Data.Void ( Void )
import Control.Monad (void)
import Types (Action (..))
import Data.Char (toLower, toUpper)
import Data.List.Extra (trim)


-- Parsers go here
type Parser = Parsec Void String


-- * Parsers

-- | Here be some parsers of Actions
pRemove, pRemovePerson, pPush, pPop, pReset :: Parser Action
pAddPerson, pAction, pAddExplicit, pClear :: Parser Action

-- | Parses 'RemoveTop' 'Action's
pRemove = do
  void (char' 'x') <|> void (string' "remove")
  return RemoveTop

-- | Parses 'RemovePerson' 'Action's
pRemovePerson = do
  void (char' 'x') <|> void (string' "remove")
  space
  name <- some printChar
  return (RemovePerson name)

-- | Parses 'Push' 'Action's
pPush = string' "push" >> return Push

-- | Parses 'Pop' 'Action's
pPop = string' "pop" >> return Pop

-- | Parses 'ResetAll' 'Action's
pReset = string' "reset" >> return ResetAll

-- | Parses 'Clear' 'Action's
pClear = do
  void (string' "clear") <|> void (string' "c")
  eof
  return Clear

-- | Parses 'AddPerson' 'Action's with explicit add
pAddExplicit = do
  string' "add"
  space
  name <- some printChar
  return (AddPerson name)

-- | Parses 'AddPerson' 'Action's without explicit add
pAddPerson = AddPerson <$> some printChar

-- | List of action parsers
actionParsers :: [Parser Action]
actionParsers = [pRemovePerson, pRemove, pPush, pPop, pClear, pReset, pAddExplicit, pAddPerson]

-- | Parser for any action
pAction = foldr1 (<|>) (map try actionParsers)

-- | trim and lowercase a string (done before parsing)
sanitize :: String -> String
sanitize = map toLower . trim

parseInput :: String -> Either (ParseErrorBundle String Void) Action
parseInput = parse pAction . sanitize $ "input"