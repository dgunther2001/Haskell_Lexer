import Data.Char (isDigit, isSpace, isAlpha, isAlphaNum)

data Token
  = TokenNumber Int
  | TokenCharacter Char 
  | TokenString String
  | TokenIdentifier String
  | TokenPlus Char 
  | TokenAssign Char
  | TokenSemiColon Char
  deriving (Show, Eq) -- enables conversion to strings, and equality operators

lexer :: String -> Either String [Token]
lexer [] = Right []
lexer (current_character:rest_of_input)
  | isSpace current_character = 
    lexer rest_of_input
  | isDigit current_character = 
    let (digits, rest) = span isDigit (current_character:rest_of_input) 
    in case lexer rest of 
      Right tokens -> Right (TokenNumber (read digits) : tokens)
      Left err -> Left err
  | current_character == '\'' =
    lexCharacterLiteral rest_of_input
  | current_character == '"' =
    lexStringLiteral rest_of_input
  | isAlpha current_character || current_character == '_' =
    let (ident, rest) = span (\next_char -> isAlphaNum next_char || next_char =='_') (current_character:rest_of_input)
    in case lexer rest of 
      Right tokens -> Right (TokenIdentifier ident : tokens)
      Left err -> Left err
  | current_character == '+' =
    fmap (TokenPlus current_character :) (lexer rest_of_input)
  | current_character == '=' =
    fmap (TokenAssign current_character :) (lexer rest_of_input)
  | current_character == ';' = 
    fmap (TokenSemiColon current_character :) (lexer rest_of_input)
  | otherwise = 
    Left ("Unexpected character (" ++ [current_character] ++ ")")


lexCharacterLiteral :: String -> Either String [Token]
-- splits the current input into 2 outputs, separated by a '\'' which is removed. char_to_store is the head of the input char list (String), and rest_of_input is pattern matched to everything after the closing '\''
lexCharacterLiteral (char_to_store:'\'':rest_of_input) = 
  case lexer rest_of_input of -- recursively call the lexer on the rest of the input, and switch based on whether the return is a list of tokens, or a string (error)
    Right tokens -> Right (TokenCharacter char_to_store : tokens) -- in the case that we get a list of tokens back, append the character token to the head of the list and go back up the call stack
    Left err -> Left err -- if we are passed an error message, pass it back up the call stack
lexCharacterLiteral _ = Left "Unterminated, or improperly created character." -- if we don't see a character followed by a '\'' with any amount of input afterwards (can be 0), pass an error message back 


lexStringLiteral :: String -> Either String [Token]
lexStringLiteral input_stream = 
  let (strContent, rest_of_stream) = span (/= '"') input_stream -- splits the input into anything before '"' and after into strLiteral and rest (rest contains the '"')
  in case rest_of_stream of -- checks cases for the rest of the stream
    ('"':rest_of_stream_string_terminus_removed) ->  -- if we find it, everything following the closing '"' is stored in the remaining input
      case lexer rest_of_stream_string_terminus_removed of -- recursively call the lexer on the remaining input, and pattern match the current case when we pass back up the stack
        Right tokens -> Right (TokenString strContent : tokens) -- if the recursive call successfully generated a list of tokens, append the token string to the front of the list
        Left err -> Left err -- if the recursive lexer call returned a string (an error), propogate the error back up the call stack
    _ -> Left "Unterminated string literal"  -- if the rest of the input does not contain a closing '"' we return an error propogated back up as an optional string


main :: IO () -- indicates that main does IO (IO is a monad that allows me to interact with stateful things... files, user input, etc)
main = do -- controls side effects of doing muliple IO ops within the IO monad itself
  input <- readFile "test/test_file_1.pyrx" -- binds the resultant IO value from reading the input file into a string
  case lexer input of -- call the lexer function on input, which will recurse until all input has been consumed and switch based on the output
    Right tokens -> print tokens -- if we get a list of tokens back, print them
    Left err -> print ("Lexer error: " ++ err) -- if we get a string (Left), it means we propogated an error, so print it out
