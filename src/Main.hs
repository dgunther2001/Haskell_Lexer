import Data.Char (isDigit, isSpace, isAlpha, isAlphaNum)

data Token
  = TokenNumber Int
  -- | TokenCharacter Char 
  | TokenIdentifier String
  | TokenPlus Char 
  | TokenAssign Char
  | TokenSemiColon Char
  deriving (Show, Eq) -- enables conversion to strings, and equality operators

lexer :: String -> Either String [Token]
lexer [] = Right []
lexer (char:char_list)
  | isSpace char = 
    lexer char_list
  | isDigit char = -- checks if the current character is a digit
    let (digits, rest) = span isDigit (char:char_list) -- collects all contiguos integers in the input string
    in case lexer rest of  -- constructs a TokenNumber on digits, and then calls the lexer on the rest of the input
      Right tokens -> Right (TokenNumber (read digits) : tokens)
      Left err -> Left err
  | isAlpha char || char == '_' =
    let (ident, rest) = span (\next_char -> isAlphaNum next_char || next_char =='_') (char:char_list)
    in case lexer rest of 
      Right tokens -> Right (TokenIdentifier ident : tokens)
      Left err -> Left err
  | char == '+' =
    fmap (TokenPlus char :) (lexer char_list)
  | char == '=' =
    fmap (TokenAssign char :) (lexer char_list)
  | char == ';' = 
    fmap (TokenSemiColon char :) (lexer char_list)
  | otherwise = 
    Left $ "Unexpected character: " ++ [char]

main :: IO ()
main = do
  let input = "123 44 = ; + 37 daniel"
  case lexer input of 
    Right tokens -> print tokens
    Left err -> putStrLn $ "Lexer error " ++ err
