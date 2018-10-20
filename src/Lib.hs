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

pat = undefined

expr = undefined

ident = identFirstChar <:> many identRestChar
identFirstChar = letter <|> char '\'' <|> char '-' <|> char '_'
identRestChar = identFirstChar <|> digit
  
parens = between (char '(' >> spaces) (spaces >> char ')')

spaces1 = many1 space

(<++>) :: Monad t => t [a] -> t [a] -> t [a]
(<++>) = liftM2 (++)

(<:>) :: Monad t => t a -> t [a] -> t [a]
(<:>) = liftM2 (:)

{-
ops 	→ 	op1 , … , opn 	    (n ≥ 1)
vars 	→ 	var1 , …, varn 	    (n ≥ 1)
fixity 	→ 	infixl | infixr | infix
 
type 	→ 	btype [-> type] 	    (function type)
 
btype 	→ 	[btype] atype 	    (type application)
 
atype 	→ 	gtycon
	| 	tyvar
	| 	( type1 , … , typek ) 	    (tuple type, k ≥ 2)
	| 	[ type ] 	    (list type)
	| 	( type ) 	    (parenthesized constructor)
 
gtycon 	→ 	qtycon
	| 	() 	    (unit type)
	| 	[] 	    (list constructor)
	| 	(->) 	    (function constructor)
	| 	(,{,}) 	    (tupling constructors)
 
context 	→ 	class
	| 	( class1 , … , classn ) 	    (n ≥ 0)
class 	→ 	qtycls tyvar
	| 	qtycls ( tyvar atype1 … atypen ) 	    (n ≥ 1)
scontext 	→ 	simpleclass
	| 	( simpleclass1 , … , simpleclassn ) 	    (n ≥ 0)
simpleclass 	→ 	qtycls tyvar
 
simpletype 	→ 	tycon tyvar1 … tyvark 	    (k ≥ 0)
constrs 	→ 	constr1 | … | constrn 	    (n ≥ 1)
constr 	→ 	con [!] atype1 … [!] atypek 	    (arity con  =  k, k ≥ 0)
	| 	(btype | ! atype) conop (btype | ! atype) 	    (infix conop)
	| 	con { fielddecl1 , … , fielddecln } 	    (n ≥ 0)
newconstr 	→ 	con atype
	| 	con { var :: type }
fielddecl 	→ 	vars :: (type | ! atype)
deriving 	→ 	deriving (dclass | (dclass1, … , dclassn)) 	    (n ≥ 0)
dclass 	→ 	qtycls
 
inst 	→ 	gtycon
	| 	( gtycon tyvar1 … tyvark ) 	    (k ≥ 0, tyvars distinct)
	| 	( tyvar1 , … , tyvark ) 	    (k ≥ 2, tyvars distinct)
	| 	[ tyvar ]
	| 	( tyvar1 -> tyvar2 ) 	    tyvar1 and tyvar2 distinct
 
fdecl 	→ 	import callconv [safety] impent var :: ftype 	    (define variable)
	| 	export callconv expent var :: ftype 	    (expose variable)
callconv 	→ 	ccall | stdcall | cplusplus 	    (calling convention)
	| 	jvm | dotnet
	| 	 system-specific calling conventions
impent 	→ 	[string] 	    (see Section 8.5.1)
expent 	→ 	[string] 	    (see Section 8.5.1)
safety 	→ 	unsafe | safe
 
ftype 	→ 	frtype
	| 	fatype  →  ftype
frtype 	→ 	fatype
	| 	()
fatype 	→ 	qtycon atype1 … atypek 	    (k  ≥  0)
 
funlhs 	→ 	var apat { apat }
	| 	pat varop pat
	| 	( funlhs ) apat { apat }
 
rhs 	→ 	= exp [where decls]
	| 	gdrhs [where decls]
 
gdrhs 	→ 	guards = exp [gdrhs]
 
guards 	→ 	| guard1, …, guardn 	    (n ≥ 1)
guard 	→ 	pat <- infixexp 	    (pattern guard)
	| 	let decls 	    (local declaration)
	| 	infixexp 	    (boolean guard)
 
exp 	→ 	infixexp :: [context =>] type 	    (expression type signature)
	| 	infixexp
 
infixexp 	→ 	lexp qop infixexp 	    (infix operator application)
	| 	- infixexp 	    (prefix negation)
	| 	lexp
 
lexp 	→ 	\ apat1 … apatn -> exp 	    (lambda abstraction, n ≥ 1)
	| 	let decls in exp 	    (let expression)
	| 	if exp [;] then exp [;] else exp 	    (conditional)
	| 	case exp of { alts } 	    (case expression)
	| 	do { stmts } 	    (do expression)
	| 	fexp
fexp 	→ 	[fexp] aexp 	    (function application)
 
aexp 	→ 	qvar 	    (variable)
	| 	gcon 	    (general constructor)
	| 	literal
	| 	( exp ) 	    (parenthesized expression)
	| 	( exp1 , … , expk ) 	    (tuple, k ≥ 2)
	| 	[ exp1 , … , expk ] 	    (list, k ≥ 1)
	| 	[ exp1 [, exp2] .. [exp3] ] 	    (arithmetic sequence)
	| 	[ exp | qual1 , … , qualn ] 	    (list comprehension, n ≥ 1)
	| 	( infixexp qop ) 	    (left section)
	| 	( qop⟨-⟩ infixexp ) 	    (right section)
	| 	qcon { fbind1 , … , fbindn } 	    (labeled construction, n ≥ 0)
	| 	aexp⟨qcon⟩ { fbind1 , … , fbindn } 	    (labeled update, n  ≥  1)
 
qual 	→ 	pat <- exp 	    (generator)
	| 	let decls 	    (local declaration)
	| 	exp 	    (guard)
 
alts 	→ 	alt1 ; … ; altn 	    (n ≥ 1)
alt 	→ 	pat -> exp [where decls]
	| 	pat gdpat [where decls]
	| 		    (empty alternative)
 
gdpat 	→ 	guards -> exp [ gdpat ]
 
stmts 	→ 	stmt1 … stmtn exp [;] 	    (n ≥ 0)
stmt 	→ 	exp ;
	| 	pat <- exp ;
	| 	let decls ;
	| 	; 	    (empty statement)
 
fbind 	→ 	qvar = exp
 
pat 	→ 	lpat qconop pat 	    (infix constructor)
	| 	lpat
 
lpat 	→ 	apat
	| 	- (integer | float) 	    (negative literal)
	| 	gcon apat1 … apatk 	    (arity gcon  =  k, k ≥ 1)
 
apat 	→ 	var [ @ apat] 	    (as pattern)
	| 	gcon 	    (arity gcon  =  0)
	| 	qcon { fpat1 , … , fpatk } 	    (labeled pattern, k ≥ 0)
	| 	literal
	| 	_ 	    (wildcard)
	| 	( pat ) 	    (parenthesized pattern)
	| 	( pat1 , … , patk ) 	    (tuple pattern, k ≥ 2)
	| 	[ pat1 , … , patk ] 	    (list pattern, k ≥ 1)
	| 	~ apat 	    (irrefutable pattern)
 
fpat 	→ 	qvar = pat
 
gcon 	→ 	()
	| 	[]
	| 	(,{,})
	| 	qcon
 
var 	→ 	varid | ( varsym ) 	    (variable)
qvar 	→ 	qvarid | ( qvarsym ) 	    (qualified variable)
con 	→ 	conid | ( consym ) 	    (constructor)
qcon 	→ 	qconid | ( gconsym ) 	    (qualified constructor)
varop 	→ 	varsym | `  varid ` 	    (variable operator)
qvarop 	→ 	qvarsym | `  qvarid ` 	    (qualified variable operator)
conop 	→ 	consym | `  conid ` 	    (constructor operator)
qconop 	→ 	gconsym | `  qconid ` 	    (qualified constructor operator)
op 	→ 	varop | conop 	    (operator)
qop 	→ 	qvarop | qconop 	    (qualified operator)
gconsym 	→ 	: | qconsym 



            

data Expr = Nil
          | Num String
          | Str String
          | Bool Bool
          | Var String
          | App Expr Expr
          | If Expr Expr Expr
          | Lam String Expr
          | Let [(String, Expr)] Expr
          -- | TypeAscript ...
          -- | Cast ...
          | New String [Expr]
          -- | Match Expr [(Patt, Expr)]
  deriving (Show, Eq)

and' a b = a && b

isBracket c = elem c "()[]{}"

(<:>) = liftM2 (:)

(<++>) = liftM2 (++)

spaces1 = skipMany1 space

symbol = satisfy (\c -> and [ any (\pred -> pred c)
                                  [isMark, isPunctuation, isSymbol]
                            , not (isBracket c)
                            , not (c == '"') ])

identFirstChar = letter <|> symbol
identRestChar = identFirstChar <|> digit
ident = identFirstChar <:> many identRestChar

escaped :: Parsec String () String
escaped = do
  char '\\'
  c <- anyChar
  return ['\\', c]

parens = between (char '(' >> spaces) (spaces >> char ')') 

nil = fmap (const Nil) (string "nil")

num = fmap Num (many1 digit <++> option "" (char '.' <:> many1 digit))

str' = do
  char '"'
  s <- many (escaped <|> fmap (\c -> [c]) (noneOf ['"']))
  char '"'
  return (concat s)

str = fmap Str str'

bool :: Parsec String u Expr
bool = fmap Bool ((<|>) (fmap (const True) (string "true"))
                        (fmap (const False) (string "false")))

var = fmap Var ident

app = parens (do rator <- expr
                 rands <- many1 (spaces1 >> expr)
                 return (foldl App rator rands))

if' = parens (do string "if"
                 spaces1
                 pred <- expr
                 spaces1
                 conseq <- expr
                 spaces1
                 alt <- expr
                 return (If pred conseq alt))

lam = parens (do string "lambda"
                 spaces1
                 params <- parens (sepEndBy1 ident spaces1)
                 spaces1
                 body <- expr
                 return (foldr Lam body params))

let' = parens (do string "let"
                  spaces1
                  binds <- bindings
                  spaces1
                  body <- expr
                  return (Let binds body))
  where bindings = parens ((>>) spaces (sepEndBy binding spaces))
        binding = parens (liftM2 (,) ident (spaces1 >> expr))

new = parens (do string "new"
                 spaces1
                 variant <- ident
                 spaces1
                 members <- sepEndBy1 expr spaces1
                 return (New variant members))

expr = choice [nil, num, str, bool, var, app, if', lam, let', new]




-}
