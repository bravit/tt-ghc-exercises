{- Uses Parsec to parse the fulluntyped lambda calculus.
   An extension of UntypedParser in untyped.
 -}
module FullUntypedParser (parseFullUntyped) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

import Control.Monad
import Control.Monad.Error
    
import FullUntyped
import TaplError
import Context

nonleadingLetters = letter <|> digit <|> oneOf "'*&^%$"

untypedDef = LanguageDef
             { commentStart = "/*"
             , commentEnd = "*/"
             , commentLine = ""
             , nestedComments = False
             , identStart = letter
             , identLetter = nonleadingLetters
             , opStart = letter
             , opLetter = nonleadingLetters
             , reservedNames = ["lambda", "succ", "pred", "iszero", "true", "false", "if", "then", "else", "let", "in", "timesfloat"]
             , reservedOpNames = []
             , caseSensitive = True
             }
               
lexer = P.makeTokenParser untypedDef

whiteSpace = P.whiteSpace lexer
symbol = P.symbol lexer
parens = P.parens lexer
braces = P.braces lexer
dot = P.dot lexer
semi = P.semi lexer
identifier = P.identifier lexer
reserved = P.reserved lexer
float = P.float lexer
stringLiteral = P.stringLiteral lexer
natural = P.natural lexer

-- --------------------
-- Zero-arg arith terms

parseTrue = reserved "true" >> return TmTrue

parseFalse = reserved "false" >> return TmFalse

parseNat = liftM numToSucc natural
    where numToSucc 0 = TmZero
          numToSucc n = TmSucc $ numToSucc (n - 1)

-- --------------------
-- One-arg arith terms

parseOneArgTerm keyword constructor = 
    reserved keyword >> liftM constructor parseTerm

parseSucc = parseOneArgTerm "succ" TmSucc

parsePred = parseOneArgTerm "pred" TmPred

parseIsZero = parseOneArgTerm "iszero" TmIsZero

-- --------------------
-- if-then-else, and the rest of the arith parser

parseIf = do reserved "if"
             pred <- parseTerm
             reserved "then"
             conseq <- parseTerm
             reserved "else"
             alt <- parseTerm
             return $ TmIf pred conseq alt

parseArithTerm = parseTrue <|>
                 parseFalse <|>
                 parseIf <|> 
                 parseSucc <|>
                 parsePred <|>
                 parseIsZero <|>
                 try parseNat

-- binds a variable into the context
binder = do var <- identifier
            bind <- (try (symbol "/" >> return NameBind)) <|>
                    (symbol "=" >> liftM TmAbbBind parseTerm)
            updateState (\c -> appendBinding c var bind)
            return $ TmBind var bind

-- Returns the index of a variable in a context.
-- Returns a GenParser, even though no parsing is done, so that
-- we can use Parsec's error handling
getIndex :: Context -> String -> GenParser Char Context Int
getIndex (Context boundNames) name = iter 0 (reverse boundNames)
    where iter _ [] = fail $ "Unbound name: " ++ name
          iter c ((n,_):ns) | n == name = return c
                            | otherwise = iter (c + 1) ns

untypedVar = do var <- identifier
                ctx <- getState
                idx <- getIndex ctx var
                return $ TmVar { index = idx, contextLength = ctxLength ctx }

-- parses, e.g., "lambda x. x x"
untypedAbs = do reserved "lambda"
                var <- identifier
                dot
                -- bind the variable before parsing the body of the 
                -- lambda abstraction
                ctx <- getState
                setState $ appendBinding ctx var NameBind

                term <- parseTerm

                -- now that we're leaving this lambda abstraction, we need
                -- to restore the original context, before "var" was bound
                setState ctx
                return $ TmAbs var term

parseLet = do reserved "let"
              var <- identifier
              symbol "="
              term <- parseTerm
              updateState (\c -> appendBinding c var (TmAbbBind term))
              reserved "in"
              body <- parseTerm
              return $ TmLet var term body
                
parseFloat = liftM TmFloat float

parseTimesfloat = do reserved "timesfloat"
                     liftM2 TmTimesfloat parseNonApp parseNonApp

parseString = liftM TmString stringLiteral

parseProj = do record <- (parens parseTerm) <|> parseRecord
               symbol "."
               str <- identifier <|> many1 digit
               return $ TmProj record str

parseRecord = braces $ liftM TmRecord $ fields 1
              where fields i = try (morefields i)
                               <|> return []
                    morefields i = do (f, i') <- onefield i
                                      (try (do fs <- commaMorefields i'
                                               return (f:fs)) <|>
                                       return [f])
                    commaMorefields i = symbol "," >> morefields i
                    onefield i = (try (namedfield i)) <|> unnamedfield i
                    namedfield i = do var <- identifier
                                      symbol "="
                                      term <- parseTerm
                                      return ((var, term), i)
                    unnamedfield i = do term <- parseTerm
                                        return ((show i, term), i + 1)

-- Parses everything expect for an application, which we handle 
-- separately to handle associativity (and to avoid infinite loops!)
parseNonApp = try binder
            <|> untypedVar
            <|> untypedAbs
            <|> (try parseFloat)
            <|> parseTimesfloat
            <|> parseArithTerm
            <|> parseLet
            <|> parseString
            <|> try parseProj
            <|> parseRecord
            <|> parens parseTerm

-- chainl1 gives us left-associativity of applications
parseTerm = chainl1 parseNonApp (return TmApp)

parseStmt = do t <- parseTerm
               semi
               return t

untypedParser = do setState newContext
                   whiteSpace
                   terms <- many1 parseStmt
                   return terms

parseFullUntyped :: String -> ThrowsError [Term]
parseFullUntyped str = case runParser untypedParser newContext "FullUntyped Parser" str of
                         Left err  -> throwError $ ParserError $ show err
                         Right val -> return val