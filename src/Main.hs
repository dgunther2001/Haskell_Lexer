import Data.Char (isDigit, isSpace, isAlpha, isAlphaNum)

data Token
  = TokenNumberLit Int
  | TokenCharacterLit Char 
  | TokenStringLit String
  | TokenIdentifier String
  | TokenPlus 
  | TokenAssign 
  | TokenSemiColon 
  | TokenInt 
  | TokenFloat 
  | TokenChar 
  | TokenString 
  | TokenBool 
  deriving (Show, Eq) -- enables conversion to strings, and equality operators

keywords :: [(String, Token)]
keywords = 
  [("int", TokenInt),
   ("float", TokenFloat),
   ("char", TokenChar),
   ("string", TokenString),
   ("bool", TokenBool)
  ]


lexer :: String -> Either String [Token]
lexer [] = Right []
lexer (current_character:rest_of_input)
  | isSpace current_character = 
    lexer rest_of_input
  | isDigit current_character = 
    let (digits, rest) = span isDigit (current_character:rest_of_input) 
    in case lexer rest of 
      Right tokens -> Right (TokenNumberLit (read digits) : tokens)
      Left err -> Left err
  | current_character == '\'' =
    lexCharacterLiteral rest_of_input
  | current_character == '"' =
    lexStringLiteral rest_of_input
  | isAlpha current_character || current_character == '_' =
    lexKeywordOrIdentifier (current_character : rest_of_input)
  | current_character == '+' =
    fmap (TokenPlus :) (lexer rest_of_input)
  | current_character == '=' =
    fmap (TokenAssign :) (lexer rest_of_input)
  | current_character == ';' = 
    fmap (TokenSemiColon :) (lexer rest_of_input)
  | otherwise = 
    Left ("Unexpected character (" ++ [current_character] ++ ")")


keywordToToken :: String -> Token -- helper function to pattern match keywords
keywordToToken identifier = 
  case lookup identifier keywords of -- lookup the current identifier in the list of keywords
    Just token -> token -- if a token was found in the keywords list, return that token
    Nothing -> TokenIdentifier identifier -- otherwise return an identifer token if it wasn't found in the kewords list


lexKeywordOrIdentifier :: String -> Either String [Token]
lexKeywordOrIdentifier input_stream = -- takes in the rest of th input 
  -- identContent is everything including alphanumerics and '_', while rest_of_stream is everything afterwards
  let (identContent, rest_of_stream) = span (\current_char -> isAlphaNum current_char || current_char == '_') input_stream 
  in case lexer rest_of_stream of -- recursively call the lexer on the rest of the input stream
    Right tokens -> Right (keywordToToken identContent : tokens) -- call the keywordToToken function to pattern match based on whether a keyword was found
    Left err -> Left err -- if we get an error message, propogate it back


lexCharacterLiteral :: String -> Either String [Token]
-- splits the current input into 2 outputs, separated by a '\'' which is removed. char_to_store is the head of the input char list (String), and rest_of_input is pattern matched to everything after the closing '\''
lexCharacterLiteral (char_to_store:'\'':rest_of_input) = 
  case lexer rest_of_input of -- recursively call the lexer on the rest of the input, and switch based on whether the return is a list of tokens, or a string (error)
    Right tokens -> Right (TokenCharacterLit char_to_store : tokens) -- in the case that we get a list of tokens back, append the character token to the head of the list and go back up the call stack
    Left err -> Left err -- if we are passed an error message, pass it back up the call stack
lexCharacterLiteral _ = Left "Unterminated, or improperly created character." -- if we don't see a character followed by a '\'' with any amount of input afterwards (can be 0), pass an error message back 


lexStringLiteral :: String -> Either String [Token]
lexStringLiteral input_stream = 
  let (strContent, rest_of_stream) = span (/= '"') input_stream -- splits the input into anything before '"' and after into strLiteral and rest (rest contains the '"')
  in case rest_of_stream of -- checks cases for the rest of the stream
    ('"':rest_of_stream_string_terminus_removed) ->  -- if we find it, everything following the closing '"' is stored in the remaining input
      case lexer rest_of_stream_string_terminus_removed of -- recursively call the lexer on the remaining input, and pattern match the current case when we pass back up the stack
        Right tokens -> Right (TokenStringLit strContent : tokens) -- if the recursive call successfully generated a list of tokens, append the token string to the front of the list
        Left err -> Left err -- if the recursive lexer call returned a string (an error), propogate the error back up the call stack
    _ -> Left "Unterminated string literal"  -- if the rest of the input does not contain a closing '"' we return an error propogated back up as an optional string


main :: IO () -- indicates that main does IO (IO is a monad that allows me to interact with stateful things... files, user input, etc)
main = do -- controls side effects of doing muliple IO ops within the IO monad itself
  input <- readFile "test/test_file_1.pyrx" -- binds the resultant IO value from reading the input file into a string
  case lexer input of -- call the lexer function on input, which will recurse until all input has been consumed and switch based on the output
    Right tokens -> print tokens -- if we get a list of tokens back, print them
    Left err -> print ("Lexer error: " ++ err) -- if we get a string (Left), it means we propogated an error, so print it out
