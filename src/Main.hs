import Data.Char (isDigit, isSpace)

data Token
    = TokenNumber Int
    -- | TokenCharacter Char 
    -- | TokenIdentifier String
    | TokenPlus Char 
    | TokenAssign Char
    | TokenSemiColon Char
    deriving (Show, Eq) -- enables conversion to strings, and equality operators

lexer :: String -> [Token]
lexer [] = []
lexer (char:char_list)
    | isSpace char = 
        lexer char_list
    | isDigit char = -- checks if the current character is a digit
        let (digits, rest) = span isDigit (char:char_list) -- collects all contiguos integers in the input string
        in TokenNumber (read digits) : lexer rest -- constructs a TokenNumber on digits, and then calls the lexer on the rest of the input
    | char == '+' =
        TokenPlus char : lexer char_list
    | char == '=' =
        TokenAssign char : lexer char_list
    | char == ';' = 
        TokenSemiColon char : lexer char_list
    | otherwise = 
        error("Unexpected character: " ++ [char])

main :: IO ()
main = do
    let input = "123 44 = ; + 37"
    let tokens = lexer input
    print tokens
