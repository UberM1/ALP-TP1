module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

-----------------------
-- Función para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "skip", "if", "else", "repeat", "until"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        , "++"
                        , "--"
                        ]
    }
  )

-----------------------------------
--- Parser de expresiones enteras
-----------------------------------
intexp :: Parser (Exp Int)
intexp = chainl1 subterm plusminus 

cons :: Parser (Exp Int)
cons = do i <- integer lis
          return (Const (fromIntegral i))

varparser :: Parser (Exp Int)
varparser = do i <- identifier lis
               (do reservedOp lis "--"
                   return (VarDec i)
                <|> do reservedOp lis "++"
                       return (VarInc i)
                    <|> return (Var i))

plusminus = do reservedOp lis "+"
               return Plus
            <|> do reservedOp lis "-"
                   return Minus

timesdiv = do reservedOp lis "*"
              return Times
           <|> do reservedOp lis "/"
                  return Div

unitParser :: Parser (Exp Int)
unitParser = do reservedOp lis "-"
                e <- intexp
                return (UMinus e)

subterm :: Parser (Exp Int)
subterm = chainl1 (parens lis intexp <|> unitParser <|> cons <|> varparser) timesdiv

------------------------------------
--- Parser de expresiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp = boolaux <|> parens lis boolaux

boolaux :: Parser (Exp Bool)
boolaux = chainl1 (notbool <|> btrue <|> bfalse <|> try lthan <|>  try gthan <|> try eqbool <|> try neqbool) (andbool <|> orbool)

btrue :: Parser (Exp Bool)
btrue = do reserved lis "true"
           return BTrue

bfalse :: Parser (Exp Bool)
bfalse = do reserved lis "false"
            return BFalse

lthan :: Parser (Exp Bool)
lthan = do exp1 <- intexp
           reservedOp lis "<"
           exp2 <- intexp
           return (Lt exp1 exp2)

gthan :: Parser (Exp Bool)
gthan = do exp1 <- intexp
           reservedOp lis ">"
           exp2 <- intexp
           return (Gt exp1 exp2)
           
eqbool :: Parser (Exp Bool)
eqbool = do exp1 <- intexp
            reservedOp lis "=="
            exp2 <- intexp
            return (Eq exp1 exp2)

neqbool :: Parser (Exp Bool)
neqbool = do exp1 <- intexp
             reservedOp lis "!="
             exp2 <- intexp
             return (NEq exp1 exp2)

notbool :: Parser (Exp Bool)
notbool = do reservedOp lis "!"
             bool <- boolexp
             return (Not bool)

-- Hay que revisar, probablemente tenga rec a la izq
andbool = do reservedOp lis "&&"
             return And

-- Hay que revisar, probablemente tenga rec a la izq
orbool = do reservedOp lis "||"
            return Or

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = chainl1 (skip <|> letcomm <|> try ifthen <|> ifthenelse <|> repeatcomm) seqcomm

skip :: Parser Comm
skip = do reserved lis "skip"
          return Skip

repeatcomm :: Parser Comm
repeatcomm = do reserved lis "repeat"
                reserved lis "{"
                c <- comm
                reserved lis "}"
                reserved lis "until"
                b <- boolexp
                return (RepeatUntil c b)

letcomm :: Parser Comm
letcomm = do i <- identifier lis
             reservedOp lis "="
             e <- intexp
             return (Let i e)

ifthen :: Parser Comm
ifthen = do reservedOp lis "if"
            b <- boolexp
            reserved lis "{"
            c <- comm
            reserved lis "}"
            return (IfThenElse b c Skip)

ifthenelse :: Parser Comm
ifthenelse = do reserved lis "if"
                b <- boolexp
                reserved lis "{"  
                comm1 <- comm
                reserved lis "}"
                reserved lis "else"
                reserved lis "{"
                comm2 <- comm
                reserved lis "}"
                return (IfThenElse b comm1 comm2)

seqcomm = do reservedOp lis ";"
             return Seq
------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
