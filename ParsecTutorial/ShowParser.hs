{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module ShowParser ( parseShow ) where
  import Text.ParserCombinators.Parsec
  import qualified Text.ParserCombinators.Parsec.Token as P
  import Text.ParserCombinators.Parsec.Language
  import Data.List ( intercalate )

  xml_header =  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"

  parseShow :: String -> String
  parseShow = run_parser showParser

  showParser :: Parser String
  showParser =
    list_parser <|>
    tuple_parser <|>
    try record_parser <|>
    adt_parser <|>
    number <|>
    quoted_string <?> "Parse error"

  run_parser :: Parser a -> String -> a
  run_parser p str = case parse p "" str of
    Left err -> error $ "parse error at " ++ show err
    Right val -> val

  -- <tag>...</tag>
  otag t = "<"++t++">"
  ctag t = "</"++t++">"
  tag t v = concat [otag t, v, ctag t]

  tagAttrs t attrs v = concat [otag (unwords $ [t]++(map (\(k,v) -> concat [k,"=\"",v,"\""]) attrs)),v,ctag t]

  joinNL = intercalate "\n"

  -- <list>
  -- <list-elt>...</list-elt>
  -- ...
  -- </list>
  list_parser = do
    ls <- brackets $ commaSep showParser
    return $ tag "list" $ joinNL $ map (tag "listelt") ls

  tuple_parser = do
    ls <- parens $ commaSep showParser
    return $ tag "tuple" $ unwords $ map (tag "tuple-elt") ls

  -- <record>
  -- <elt key="k">v</elt>
  -- ...
  -- </record>
  -- 
  -- key-value pairs: k = v -- v can be anything
  record_parser = do
    ti <- type_identifier
    ls <- braces $ commaSep kvparser
    return $ tagAttrs "record" [("name", ti)] (joinNL ls)

  kvparser = do
    k <- identifier
    symbol "="
    tagAttrs "elt" [("key", k)] <$> showParser

  type_identifier = do
    fst <- oneOf ['A' .. 'Z']
    rest <- many alphaNum
    whiteSpace
    return $ fst:rest

  adt_parser = do
    tag "adt" <$> type_identifier

  quoted_string = do
    s <- stringLiteral
    return $ "\""++s++"\""

  number = do
    show <$> integer

  lexer = P.makeTokenParser emptyDef

  parens = P.parens lexer
  brackets = P.brackets lexer
  braces = P.braces lexer
  commaSep = P.commaSep lexer
  whiteSpace = P.whiteSpace lexer
  symbol = P.symbol lexer
  identifier = P.identifier lexer
  integer = P.integer lexer
  stringLiteral = P.stringLiteral lexer