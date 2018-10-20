{-# LANGUAGE FlexibleContexts, TupleSections #-}

module Lib where

import Text.Parsec
import Data.Char (isMark, isPunctuation, isSymbol)
import Data.List (intercalate)
import Data.Either.Combinators (rightToMaybe)
import Control.Monad

-- for every line, remove everything after first non-string comment char
preprocess src = hindent (translate (removeComments src))

removeComments = undefined
translate = undefined
hindent = undefined

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
                  name <- ident
                  spaces1
                  exps <- option "" exports
                  return ("module " ++ name ++ " " ++ exps ++ " where"))
  spaces1
  b <- body
  return (m ++ "\n" ++ b)

exports = fmap (("("++) . (++")") . intercalate ", ") (parens (sepEndBy export spaces1))

export = ident <|> parens (do constr <- ident
                              spaces1
                              variants <- sepEndBy1 ident spaces1
                              return (constr ++ " (" ++ intercalate ", " variants ++ ")"))

body = do
  is <- impDecls
  ts <- topDecls
  return (is ++ "\n" ++ ts)

impDecls = fmap unlines (sepEndBy impDecl spaces)

impDecl = parens (do string "import"
                     q <- option "" (char '-' >> string "qualified")
                     spaces1
                     m <- (<|>) ident
                                (parens (do string "as"
                                            spaces1
                                            m <- ident
                                            spaces1
                                            alias <- ident
                                            return (m ++ " as " ++ alias)))
                     r <- option "" ((<|>) (do spaces1
                                               string "hiding"
                                               spaces1
                                               is <- imports
                                               return ("hiding " ++ is))
                                           (spaces1 >> imports))
                     return ("import " ++ q ++ " " ++ m ++ " " ++ r))

imports = fmap (("("++) . (++")") . intercalate ", ") (parens (sepEndBy import' spaces1))

import' = (<|>) ident
                (parens (do tyCon <- ident
                            spaces1
                            members <- sepEndBy1 ident spaces1
                            return (tyCon ++ " (" ++ intercalate ", " members ++ ")")))

topDecls = fmap unlines (sepEndBy topDecl spaces)

topDecl = choice [ typeDef
                 , dataDef
                 , newtypeDef
                 , classDef
                 , instanceDef
                 , decl ]

typeDef = parens (do string "type"
                     spaces1
                     lhs <- simpletype
                     spaces1
                     rhs <- type'
                     return ("type " ++ lhs ++ " = " ++ rhs))

dataDef = parens $ do
  string "data"
  spaces1
  lhs <- simpletype
  rhs <- option "" (spaces1 >> constrs)
  der <- option "" (spaces1 >> deriving')
  return ("data " ++ lhs ++ " " ++ rhs ++ " " ++ der)

constrs = fmap (("= "++) . (intercalate " | "))
               (parens (string "=" >> spaces1 >> (sepEndBy1 constr spaces1)))

constr = choice [ ident
                , parens (do string "record"
                             spaces1
                             con <- ident
                             spaces1
                             fields <- fmap (intercalate ", ") (sepEndBy1 field spaces1)
                             return (con ++ " { " ++ fields ++ " }"))
                , parens (do con <- ident
                             spaces1
                             ts <- sepEndBy1 type' spaces1
                             return (con ++ " " ++ unwords ts)) ]

newtypeDef = parens $ do
  string "newtype"
  spaces1
  lhs <- simpletype
  spaces1
  rhs <- newconstr
  der <- option "" (spaces1 >> deriving')
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

deriving' = fmap (("deriving "++) . unwords) (parens (string "deriving" >> spaces1 >> sepEndBy1 ident spaces1))

classDef = parens $ do
  string "class"
  spaces1
  lhs <- parens (ident <++> spaces1 <++> ident)
  rhs <- option "" (fmap (" where "++) (spaces1 >> decls1))
  return ("class " ++ lhs ++ rhs)

instanceDef = parens $ do
  string "instance"
  spaces1
  lhs <- parens (ident <++> spaces1 <++> ident)
  rhs <- option "" (fmap (" where "++) (spaces1 >> decls1))
  return ("class " ++ lhs ++ rhs)

decls = decls' sepEndBy

decls1 = decls' sepEndBy1

decls' sep = fmap (intercalate "; ") (sep decl spaces1)

decl = typeSig <|> def

typeSig = parens $ do
  string "::"
  spaces1
  name <- ident
  spaces1
  ty <- type'
  return (name ++ " :: " ++ ty)

def = parens $ do
  char '='
  spaces1
  lhs <- pat
  spaces1
  rhs <- expr
  return (lhs ++ " = " ++ rhs)

simpletype = ident <|> parens (do tyCon <- ident
                                  spaces1
                                  params <- sepEndBy1 ident spaces1
                                  return (tyCon ++ " " ++ unwords params))

type' = ident <|> funType <|> typeApp

funType = parens $ do
  string "->"
  spaces1
  t0 <- type'
  spaces1
  tys <- sepEndBy1 type' spaces1
  return ("(" ++ foldl (\acc t -> acc ++ " -> " ++ t) t0 tys ++ ")")

typeApp = parens $ do
  con <- type'
  spaces1
  args <- sepEndBy1 type' spaces1
  return (con ++ " " ++ unwords args)

pat :: Parsec String u String
pat = choice [ negLit
             , lit
             , wildcard
             , parens' (do char '@'
                           name <- ident
                           p <- pat
                           return (name ++ " @ " ++ p))
             , char '(' <:> spaces' <++> string ")"
             , char '[' <:> spaces' <++> string "]"
             , parens' (do char ','
                           spaces1
                           ps <- sepEndBy1 pat spaces1
                           return (intercalate ", " ps))
             , brackets' (fmap (intercalate ", ") (sepEndBy1 pat spaces1))
             , rpat ]

rpat :: Parsec String u String
rpat = parens (do string "record"
                  spaces1
                  fs <- sepEndBy1 fpat spaces1
                  return ("{" ++ intercalate ", " fs ++ "}"))

fpat :: Parsec String u String
fpat = parens (do name <- ident
                  spaces1
                  p <- pat
                  return (name ++ " = " ++ p))

negLit = parens' (char '-' <:> spaces1 <++> num)

lit = num <|> str

wildcard = string "_"

num = many1 digit <++> option "" (char '.' <:> many1 digit)

str = char '"' <:> fmap concat (many (escaped <|> fmap (\c -> [c]) (noneOf ['"']))) <++> string "\""

escaped :: Parsec String u String
escaped = do
  char '\\'
  c <- anyChar
  return ['\\', c]

ident = (identFirstChar <:> many identRestChar) <|> rator

identFirstChar = letter <|> char '_'

identRestChar = alphaNum <|> oneOf "_'"

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

expr = choice [ list
              , lit
              , ident
              , parens' pexpr]

pexpr = choice [ typeAscr
               , lam
               , let_
               , if_
               , match
               , do_
               , app
               , tuple
               , record
               , update ]

typeAscr = do
  string "::"
  spaces1
  e <- expr
  spaces1
  t <- type'
  return (e ++ " :: " ++ t)

lam = do
  string "\\"
  spaces1
  ps <- parens (sepEndBy1 pat spaces1)
  spaces1
  b <- expr
  return ("\\ " ++ unwords ps ++ " -> " ++ b)

let_ = do
  string "let"
  spaces1
  binds <- parens decls1
  spaces1
  bod <- expr
  return ("let " ++ binds ++" in " ++ bod)

if_ = do
  string "if"
  spaces1
  pred <- expr
  spaces1
  conseq <- expr
  spaces1
  alt <- expr
  return ("if " ++ pred ++ " then " ++ conseq ++ " else " ++ alt)

match = do
  string "match"
  spaces1
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
  string "do"
  spaces1
  ss <- stmts
  return ("do " ++ ss)

stmts = fmap (intercalate "; ") (sepEndBy1 stmt spaces1)

stmt = bind <|> expr

bind = parens $ do
  string "<-"
  spaces1
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
  char ','
  spaces1
  es <- fmap (intercalate ", ") (sepEndBy1 expr spaces1)
  return es
  
list = brackets' (fmap (intercalate ", ") (sepEndBy1 expr spaces1))

record = do
  string "record"
  spaces1
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
