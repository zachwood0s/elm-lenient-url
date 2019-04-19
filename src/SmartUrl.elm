module SmartUrl exposing (fromString)

import Url exposing (Url, Protocol(..))
import Parser exposing (Parser, oneOf, map, keyword, (|.), (|=), symbol, succeed)
import Regex exposing (Regex)
import Maybe

type alias SmartUrl =
  { protocol : Maybe Protocol
  , hostname : String 
  , port_    : Maybe Int 
  , path     : String 
  , query    : Maybe String 
  , fragment : Maybe String 
  }


smartUrlToUrl : SmartUrl -> Maybe Url 
smartUrlToUrl smart = 
  let
    buildUrl protocol smartUrl = 
      Url protocol smartUrl.hostname smartUrl.port_ smartUrl.path smartUrl.query smartUrl.fragment
  in
    validateHostname smart.protocol smart.hostname 
    |> mapFromBool (buildUrl (Maybe.withDefault Http smart.protocol) smart)

mapFromBool : a -> Bool -> Maybe a
mapFromBool elem succeeded =
  if succeeded then Just elem
  else Nothing

fromString : String -> Maybe Url
fromString str 
  = Parser.run urlParser str 
    |> Result.toMaybe 
    |> Maybe.andThen smartUrlToUrl
  
urlParser : Parser SmartUrl 
urlParser = 
  succeed SmartUrl 
    |= optionally chompProtocol
    |= chompHostname
    |= optionally chompPort
    |= chompPath
    |= optionally chompQuery
    |= optionally chompFragment
    |. Parser.end

chompProtocol : Parser Protocol
chompProtocol =
  oneOf
    [ map (\_ -> Http) (symbol "http://")
    , map (\_ -> Https) (symbol "https://")
    ]

validateHostname : Maybe Protocol -> String -> Bool 
validateHostname protocol authority =
  case protocol of 
    Just _ -> Regex.contains hostnameValid authority 
    Nothing -> Regex.contains hostnameValidNoProtocol authority

chompHostname : Parser String 
chompHostname =
    Parser.getChompedString (Parser.chompWhile (\c -> Char.isAlphaNum c || c == '.'))

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

chompQuery : Parser String
chompQuery = 
  Parser.succeed identity
    |. symbol "?"
    |= Parser.getChompedString (Parser.chompUntilEndOr "#")

chompFragment : Parser String
chompFragment = 
  Parser.succeed identity 
    |. symbol "#"
    |= Parser.getChompedString (Parser.chompWhile (\_ -> True))

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
hostnameValidNoProtocol : Regex 
hostnameValidNoProtocol = 
  Maybe.withDefault Regex.never <|
    Regex.fromString "^(([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\\-]*[a-zA-Z0-9])\\.)+([A-Za-z0-9]|[A-Za-z0-9][A-Za-z0-9\\-]*[A-Za-z0-9])$"

hostnameValid : Regex 
hostnameValid = 
  Maybe.withDefault Regex.never <|
    Regex.fromString "^(([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\\-]*[a-zA-Z0-9])\\.)*([A-Za-z0-9]|[A-Za-z0-9][A-Za-z0-9\\-]*[A-Za-z0-9])$"

