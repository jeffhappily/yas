{-# LANGUAGE OverloadedStrings #-}
module Eval where

import           Control.Exception
import           Control.Monad.Reader
-- import Control.Monad.Resource
import qualified Data.Map             as Map
import qualified Data.Text            as T
import           LispVal
import           Parser

basicEnv :: Map.Map T.Text LispVal
basicEnv = Map.empty

evalFile :: T.Text -> IO () --program file
evalFile fileExpr = (runASTinEnv basicEnv $ fileToEvalForm fileExpr)
                     >>= print

fileToEvalForm :: T.Text -> Eval LispVal
fileToEvalForm input = either (throw . PError . show )
                              evalBody
                              $ readExprFile input

runParseTest :: T.Text -> T.Text -- for view AST
runParseTest input = either (T.pack . show)
                            (T.pack . show)
                            $ readExpr input

runASTinEnv :: EnvCtx -> Eval b -> IO b
runASTinEnv code action = runReaderT (unEval action) code

eval :: LispVal -> Eval LispVal
eval (List [Atom "quote", val]) = return val
eval (Number i) = return $ Number i
eval (String s) = return $ String s
eval (Bool b)   = return $ Bool b
eval (List [])  = return Nil
eval Nil        = return Nil
eval (List [Atom "write", rest]) =
  return . String . T.pack $ show rest
eval (List ((:) (Atom "write") rest)) =
  return . String . T.pack . show $ List rest
eval n@(Atom _) = getVar n
eval (List [Atom "if", pred', truExpr, flsExpr]) = do
  ifRes <- eval pred'
  case ifRes of
      (Bool True)  -> eval truExpr
      (Bool False) -> eval flsExpr
      _            -> throw $ BadSpecialForm "if"
eval (List [Atom "let", List pairs, expr]) = do
  env   <- ask
  atoms <- mapM ensureAtom $ getFirst  pairs
  vals  <- mapM eval       $ getSecond pairs
  let env' = Map.fromList (Prelude.zipWith (\a b -> (extractVar a, b)) atoms vals) <> env
  local (const env')  $ evalBody expr
eval (List [Atom "begin", rest]) = evalBody rest
eval (List ((:) (Atom "begin") rest )) = evalBody $ List rest
eval (List [Atom "define", varExpr, expr]) = do
  varAtom <- ensureAtom varExpr
  evalVal <- eval expr
  env     <- ask
  let envFn = const $ Map.insert (extractVar varAtom) evalVal env
  local envFn $ return varExpr
eval (List [Atom "lambda", List params, expr]) = do
  envLocal <- ask
  return  $ Lambda (IFunc $ applyLambda expr params) envLocal
eval (List (Atom "lambda":_) ) = throw $ BadSpecialForm "lambda"
eval (List ((:) x xs)) = do
  funVar <- eval x
  xVal   <- mapM eval xs
  case funVar of
      (Fun (IFunc internalFn)) -> internalFn xVal
      (Lambda (IFunc internalfn) boundenv) -> local (const boundenv)
                                                   $ internalfn xVal
      _                -> throw $ NotFunction funVar
eval (List [Atom "delay", expr]) = do
  envLocal <- ask
  return $ Lambda (IFunc $ applyLambda expr []) envLocal

applyLambda :: LispVal -> [LispVal] -> [LispVal] -> Eval LispVal
applyLambda expr params args = do
  env <- ask
  argEval <- mapM eval args
  let env' = Map.fromList (Prelude.zipWith (\a b -> (extractVar a,b)) params argEval) <> env
  local (const env') $ eval expr

evalBody :: LispVal -> Eval LispVal
evalBody (List [List ((:) (Atom "define") [Atom var, defExpr]), rest]) = do
  evalVal <- eval defExpr
  env     <- ask
  local (const $ Map.insert var evalVal env) $ eval rest
evalBody (List ((:) (List ((:) (Atom "define") [Atom var, defExpr])) rest)) = do
  evalVal <- eval defExpr
  env     <- ask
  let envFn = const $ Map.insert var evalVal env
  local envFn $ evalBody $ List rest
evalBody x = eval x

getFirst :: [LispVal] -> [LispVal]
getFirst []                 = []
getFirst (List [x, y] : xs) = x : getFirst xs
getFirst _                  = throw $ BadSpecialForm "let"

getSecond :: [LispVal] -> [LispVal]
getSecond []                 = []
getSecond (List [x, y] : xs) = y : getSecond xs
getSecond _                  = throw $ BadSpecialForm "let"

ensureAtom :: LispVal -> Eval LispVal
ensureAtom n@(Atom _) = return  n
ensureAtom n          = throw $ TypeMismatch "atom" n

extractVar :: LispVal -> T.Text
extractVar (Atom atom) = atom

getVar :: LispVal ->  Eval LispVal
getVar (Atom atom) = do
  env <- ask
  case Map.lookup atom env of
      Just x  -> return x
      Nothing -> throw $ UnboundVar atom
