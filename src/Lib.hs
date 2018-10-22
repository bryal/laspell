{-# LANGUAGE FlexibleContexts, TupleSections #-}

module Lib where

import Text.Parsec
import Data.Char (isMark, isPunctuation, isSymbol)
import Data.List (intercalate)
import Data.Either
import Control.Monad
import qualified HIndent as H
import qualified HIndent.Types as HT
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Binary.Builder (toLazyByteString)

-- for every line, remove everything after first non-string comment char
preprocess filename src = hindent (translate filename (removeComments src))

removeComments src = rc src False
  where rc ('\n':src)    _     = '\n' : (rc src False)
        rc (_:src)       True  = rc src True
        rc ('-':'-':src) _     = rc src True
        rc (c:src)       False = c : (rc src False)
        rc ""            _     = ""

translate :: FilePath -> String -> String
translate filename src = case (parse file filename src) of
  Right s -> s
  Left e -> error ("Error translating: " ++ show e)

hindent :: String -> ByteString
hindent src = case (H.reformat HT.defaultConfig Nothing Nothing (pack src)) of
  Right b -> toLazyByteString b
  Left e -> error ("Error applying HIndent: " ++ e)

file = do
  spaces
  ps <- pragmas
  spaces
  m <- module'
  return (unlines ps ++ "\n" ++ m)

pragmas :: Parsec String u [String]
pragmas = fmap (fmap (\s -> "{-#" ++ s ++ "#-}")) (sepEndBy pragma spaces)

pragma :: Parsec String u String
pragma = string "{-#" >> manyTill anyChar (string "#-}")

module' = do
  m <- parens (do string "module"
                  spaces1
                  name <- ident
                  exps <- option "" (spaces1 >> exports)
                  return ("module " ++ name ++ " " ++ exps ++ " where"))
  spaces
  b <- body
  return (m ++ "\n" ++ b)

exports = fmap (("("++) . (++")") . intercalate ", ") (parens (sepEndBy export spaces1))

export = ident <|> parens (do constr <- ident
                              spaces1
                              variants <- sepEndBy1 ident spaces1
                              return (constr ++ " (" ++ intercalate ", " variants ++ ")"))

body = do (fmap unlines (sepEndBy (parens (impDecl <|> topDecl)) spaces))

impDecl = do
  string "import"
  q <- option "" (char '-' >> string "qualified")
  spaces1
  m <- (<|>) ident
             (parens (do string "as"
                         spaces1
                         m <- ident
                         spaces1
                         alias <- ident
                         return (m ++ " as " ++ alias)))
  r <- option "" (spaces1 >> ((<|>) (do string "hiding"
                                        spaces1
                                        is <- imports
                                        return ("hiding " ++ is))
                                    imports))
  return ("import " ++ q ++ " " ++ m ++ " " ++ r)

imports = fmap (("("++) . (++")") . intercalate ", ") (parens (sepEndBy import' spaces1))

import' = (<|>) ident
                (parens (do tyCon <- ident
                            spaces1
                            members <- sepEndBy1 ident spaces1
                            return (tyCon ++ " (" ++ intercalate ", " members ++ ")")))

topDecl = choice [ typeDef
                 , dataDef
                 , newtypeDef
                 , classDef
                 , instanceDef
                 , decl' ]

typeDef = do
  string "type"
  spaces1
  lhs <- simpletype
  spaces1
  rhs <- type'
  return ("type " ++ lhs ++ " = " ++ rhs)

dataDef = do
  string "data"
  spaces1
  lhs <- simpletype
  rhs <- option [] (spaces1 >> sepEndBy (parens (constrs <|> deriving')) spaces1)
  return ("data " ++ lhs ++ " " ++ unwords rhs)

constrs = fmap (("= "++) . (intercalate " | "))
               (string "=" >> spaces1 >> (sepEndBy1 constr spaces1))

constr = ident <|> parens pconstr

pconstr = (<|>) (do string "record"
                    spaces1
                    con <- ident
                    spaces1
                    fields <- fmap (intercalate ", ") (sepEndBy1 field spaces1)
                    return (con ++ " { " ++ fields ++ " }"))
                (do con <- ident
                    spaces1
                    ts <- sepEndBy1 type' spaces1
                    return (con ++ " " ++ unwords ts))

newtypeDef = do
  string "newtype"
  spaces1
  lhs <- simpletype
  spaces1
  rhs <- newconstr
  der <- option "" (spaces1 >> parens deriving')
  return ("newtype " ++ lhs ++ " = " ++ rhs ++ " " ++ der)

newconstr = parens ((<|>) (do string "record"
                              spaces1
                              con <- ident
                              spaces1
                              f <- field
                              return (con ++ " { " ++ f ++ " }"))
                          (do con <- ident
                              spaces1
                              t <- type'
                              return (con ++ " " ++ t)))

field = parens (do name <- ident
                   spaces1
                   t <- type'
                   return (name ++ " = " ++ t))

deriving' = fmap (("deriving ("++) . (++")") . (intercalate ", ")) (string "deriving" >> spaces1 >> sepEndBy1 ident spaces1)

classDef = do
  string "class"
  spaces1
  lhs <- parens (ident <++> spaces1 <++> ident)
  rhs <- option "" (fmap (" where "++) (spaces1 >> decls1))
  return ("class " ++ lhs ++ rhs)

instanceDef = do
  string "instance"
  spaces1
  lhs <- parens (ident <++> spaces1 <++> ident)
  rhs <- option "" (fmap (" where "++) (spaces1 >> decls1))
  return ("class " ++ lhs ++ rhs)

decls = decls' sepEndBy

decls1 = decls' sepEndBy1

decls' sep = fmap (intercalate "; ") (sep decl spaces1)

decl = parens decl'

decl' = typeSig <|> def

typeSig = do
  string "::"
  spaces1
  name <- ident
  spaces1
  ty <- type'
  return (name ++ " :: " ++ ty)

def = do
  char '='
  spaces1
  lhs <- lhsPat
  spaces1
  rhs <- expr
  return (lhs ++ " = " ++ rhs)

simpletype = ident <|> parens (do tyCon <- ident
                                  spaces1
                                  params <- sepEndBy1 ident spaces1
                                  return (tyCon ++ " " ++ unwords params))

type' = choice [ ident
               , parens' ptype
               , listType ]

-- type in parens
ptype = choice [ funType
               , tupType
               , typeApp
               , spaces' ]

funType = do
  string "->"
  spaces1
  t0 <- type'
  spaces1
  tys <- sepEndBy1 type' spaces1
  return (foldl (\acc t -> acc ++ " -> " ++ t) t0 tys)

tupType = do
  char ','
  spaces1
  ts <- sepEndBy1 type' spaces1
  return (intercalate ", " ts)

typeApp = do
  con <- type'
  spaces1
  args <- sepEndBy1 type' spaces1
  return (con ++ " " ++ unwords args)

pat :: Parsec String u String
pat = pat' parens'

lhsPat = pat' parens

pat' par = choice [ ident
                  , lit
                  , wildcard
                  , par ppat
                  , brackets' (fmap (intercalate ", ") (sepEndBy pat spaces1))
                  , rpat ]

-- pattern in parens
ppat = choice [ negLit
              , do char '@'
                   name <- ident
                   p <- pat
                   return (name ++ " @ " ++ p)
              , do char ','
                   spaces1
                   ps <- sepEndBy1 pat spaces1
                   return (intercalate ", " ps)
              , dpat
              , spaces' ]

-- destructuring pattern
dpat = do con <- ident
          spaces1
          ps <- sepEndBy1 pat spaces1
          return (con ++ " " ++ unwords ps)

-- record pattern
rpat :: Parsec String u String
rpat = parens (do string "record"
                  spaces1
                  fs <- sepEndBy1 fpat spaces1
                  return ("{" ++ intercalate ", " fs ++ "}"))

-- record field pattern
fpat :: Parsec String u String
fpat = parens (do name <- ident
                  spaces1
                  p <- pat
                  return (name ++ " = " ++ p))

negLit = char '-' <:> spaces1 <++> num

lit = num <|> str <|> chr

wildcard = string "_"

num = many1 digit <++> option "" (char '.' <:> many1 digit)

str = char '"' <:>  fmap concat (many (escaped <|> fmap (\c -> [c]) (noneOf ['"'])))
               <++> string "\""

chr = char '\'' <:>  (escaped <|> fmap (\c -> [c]) (noneOf ['\'']))
                <++> string "'"

escaped :: Parsec String u String
escaped = do
  char '\\'
  c <- anyChar
  return ['\\', c]

ident = (identFirstChar <:> many identRestChar) <|> rator

identFirstChar = letter <|> char '_'

identRestChar = alphaNum <|> oneOf "_'."

rator = fmap (("("++) . (++")")) (many1 ratorChar)

ratorChar = oneOf ":!#$%&*+./<=>?@\\^|-~"

parens = between (char '(' >> spaces) (spaces >> char ')')

parens' = (fmap (("("++) . (++")"))) . parens

brackets :: Parsec String u a -> Parsec String u a
brackets = between (char '[' >> spaces) (spaces >> char ']')

brackets' = (fmap (("["++) . (++"]"))) . brackets

spaces1 = many1 space

spaces' = many space

(<++>) :: Monad t => t [a] -> t [a] -> t [a]
(<++>) = liftM2 (++)

(<:>) :: Monad t => t a -> t [a] -> t [a]
(<:>) = liftM2 (:)

expr = choice [ listExpr
              , lit
              , ident
              , parens' pexpr]

pexpr = choice [ typeAscr
               , lam
               , let_
               , if_
               , cond
               , match
               , do_
               , app
               , tuple
               , record
               , update ]

typeAscr = do
  try (string "::" >> spaces1)
  e <- expr
  spaces1
  t <- type'
  return (e ++ " :: " ++ t)

lam = do
  try (string "\\" >> spaces1)
  ps <- parens (sepEndBy1 pat spaces1)
  spaces1
  b <- expr
  return ("\\ " ++ unwords ps ++ " -> " ++ b)

let_ = do
  try (string "let" >> spaces1)
  binds <- parens decls1
  spaces1
  bod <- expr
  return ("let " ++ binds ++" in " ++ bod)

if_ = do
  try (string "if" >> spaces1)
  pred <- expr
  spaces1
  conseq <- expr
  spaces1
  alt <- expr
  return ("if " ++ pred ++ " then " ++ conseq ++ " else " ++ alt)

-- macro
cond = do
  try (string "cond" >> spaces1)
  (cs, e) <- clauses
  return (foldr (\(test, expr) next -> "if "++test++" then "++expr++" else ("++next++")")
                e
                cs)

clauses :: Parsec String u ([(String, String)], String)
clauses = (>>=) (parens (fmap Left elseClause <|> fmap Right clause))
                (\c -> case c of
                         Left e  -> return ([], e)
                         Right c -> fmap (\(cs, e) -> (c:cs, e))
                                         (spaces1 >> clauses))

elseClause = do
  try (string "else" >> spaces1)
  e <- expr
  return e

clause = do
  t <- expr
  spaces1
  e <- expr
  return (t, e)

match = do
  try (string "match" >> spaces1)
  e <- expr
  spaces1
  cs <- cases
  return ("case " ++ e ++ " of " ++ cs)

cases = fmap (intercalate "; ") (sepEndBy1 case_ spaces1)

case_ = parens $ do
  p <- pat
  spaces1
  b <- expr
  return (p ++ " -> " ++ b)

do_ = do
  try (string "do" >> spaces1)
  ss <- stmts
  return ("do " ++ ss)

stmts = fmap (intercalate "; ") (sepEndBy1 stmt spaces1)

stmt = parens (bind <|> pexpr) <|> expr

bind = do
  try (string "<-" >> spaces1)
  lhs <- pat
  spaces1
  rhs <- expr
  return (lhs ++ " <- " ++ rhs)

app = do
  op <- expr
  spaces1
  args <- fmap unwords (sepEndBy1 expr spaces1)
  return (op ++ " " ++ args)

tuple = do
  try (char ',' >> spaces1)
  es <- fmap (intercalate ", ") (sepEndBy1 expr spaces1)
  return es

listExpr = brackets' (fmap (intercalate ", ") (sepEndBy1 expr spaces1))

listType = brackets' type'

record = do
  try (string "record" >> spaces1)
  fs <- fmap (intercalate ", ") (sepEndBy1 fbind spaces1)
  return ("{ " ++ fs ++ " }")

fbind = parens $ do
  lhs <- ident
  spaces1
  rhs <- expr
  return (lhs ++ " = " ++ rhs)
  
update = do
  e <- expr
  spaces1
  assignments <- fmap (intercalate ", ") (sepEndBy1 fbind spaces1)
  return (e ++ " { " ++ assignments ++ " }")
