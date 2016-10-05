import Text.ParserCombinators.Parsec
import System.Environment

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
  deriving (Eq, Ord, Show)


jsonFile :: GenParser Char st JValue
jsonFile = do
  result <- jobject
  spaces
  eof
  return result

jsonElem :: GenParser Char st JValue
jsonElem = do
  spaces
  result <- jsonElem'
  spaces
  return result

jsonElem' = jsonArr
        <|> jsonDigit
        <|> jsonString
        <|> jsonBool
        <|> jsonNull
        <|> jobject
        <?> "json element"

jsonString :: GenParser Char st JValue
jsonString = jsonStringDQ <|> jsonStringSQ

jsonStringDQ = do
  char '"'
  s <- many $ noneOf "\"" -- crude.  does not allow double quotes within strings
  char '"'
  return $ JString s

jsonStringSQ = do
  char '\''
  s <- many $ noneOf "'" -- crude, same as above
  char '\''
  return $ JString s

jsonBool = do
  bStr <- string "true" <|> string "false"
  return $ case bStr of
    "true" -> JBool True
    "false" -> JBool False

jsonDigit :: GenParser Char st JValue
jsonDigit = do
  n <- many1 $ digit
  return $ JNumber $ read n

jsonNull = do
  string "null"
  return JNull

jsonArr = do
  char '['
  arr <- jsonElem `sepBy` (char ',')
  char ']'
  return $ JArray arr

objBegining = try (string "{\n")
           <|> string "{"
           <?> "JSON Element"

jobject = do
  objBegining
  obj <- pairsep `sepBy` (char ',')
  char '}'
  return $ JObject obj

pairsep = do
  key <- many1 (noneOf ":")
  string ":"
  value <- jsonElem
  return (key,value)

parseJSON :: String -> Either ParseError JValue
parseJSON input = parse jsonFile "(unknown)" input


toString :: JValue      -> String
toString (JString s)    = "\"" ++ s ++ "\""
toString (JNumber n)    = show n
toString (JBool True)   = "true"
toString (JBool False)  = "false"
toString JNull          = "null"
toString (JArray lst)   = "[" ++ list2str lst ++ "]"
-- Comment out the next line, recompile, and call toString with a JObject.  Note the error.
toString (JObject o)    = "{" ++ jobjtostr o ++ "}"

jobjtostr :: [(String, JValue)] -> String
jobjtostr [] = ""
jobjtostr (x:[]) = ( fst(x) ++ ":" ++ toString (snd(x)) )
jobjtostr (x:xs) = ( fst(x) ++ ":" ++ toString (snd(x)) ) ++ ",\n " ++ (jobjtostr xs)

list2str :: [JValue] -> String
list2str [] = ""
list2str (x:[]) = (toString x)
list2str (x:xs) = (toString x) ++ ",\n " ++ (list2str xs)

isNull JNull  = True
-- The following 5 lines are replaced with the _ version
--isNull (JString s) = False
--isNull (JNumber n) = False
--isNull (JBool b)   = False
--isNull (JObject o) = False
--isNull (JArray a)  = False
isNull _      = False

main = do
  args <- getArgs
  p <- parseFromFile jsonFile (head args)
  case p of
    Left err  -> print err
    Right json -> putStrLn $ toString json
