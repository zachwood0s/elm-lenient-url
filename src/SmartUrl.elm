module SmartUrl exposing (fromString)

import Url exposing (Url, Protocol(..))
import Parser exposing (Parser, oneOf, map, keyword, (|.), (|=), symbol, succeed)
import Regex exposing (Regex)
import Maybe

fromString str = Parser.run urlParser str
  
urlParser : Parser Url 
urlParser = 
  succeed Url 
    |= Parser.map (Maybe.withDefault Http) (optionally chompProtocol)
    |= chompHostname
    |= optionally chompPort
    |= chompPath
    |= Parser.map (\_ -> Nothing) (succeed "")
    |= Parser.map (\_ -> Nothing) (succeed "")

chompProtocol : Parser Protocol
chompProtocol =
  oneOf
    [ map (\_ -> Http) (symbol "http://")
    , map (\_ -> Https) (symbol "https://")
    ]

chompHostname : Parser String 
chompHostname =
  let 
    valid authority =
      if Regex.contains hostnameValid authority then
        Parser.succeed authority 
      else 
        Parser.problem "Invalid hostname!"
  in
    Parser.getChompedString (Parser.chompWhile (\c -> Char.isAlphaNum c || c == '.'))
      |> Parser.andThen valid

chompPort : Parser Int
chompPort =
  Parser.succeed identity  
    |. symbol ":"
    |= Parser.int 

chompPath : Parser String 
chompPath = 
  let 
    makeValid str = 
      case str of 
        "" -> succeed "/"
        _ -> succeed str
  in
    Parser.getChompedString (Parser.chompWhile (\c -> c /= '?' && c /= '#'))
      |> Parser.andThen makeValid

optionally : Parser a -> Parser (Maybe a)
optionally parser =
  oneOf 
    [ parser |> map Just 
    , succeed Nothing
    ]

-- Regex Validations

{- Based on this stackoverflow post: https://stackoverflow.com/questions/106179/regular-expression-to-match-dns-hostname-or-ip-address
   Made a few minor tweaks to require at least one '.' in it. This is so we know its actually attempting to reference a site.
   e.g. before it would match "google" but now it requires "google.com" or something similar
-}
hostnameValid : Regex 
hostnameValid = 
  Maybe.withDefault Regex.never <|
    Regex.fromString "^(([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\\-]*[a-zA-Z0-9])\\.)+([A-Za-z0-9]|[A-Za-z0-9][A-Za-z0-9\\-]*[A-Za-z0-9])$"