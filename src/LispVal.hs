{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LispVal where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Dynamic (Typeable)
import qualified Data.Map as Map
import qualified Data.Text as T

data LispVal
  = Atom T.Text
  | List [LispVal]
  | Number Integer
  | String T.Text
  | Fun IFunc
  | Lambda IFunc EnvCtx
  | Nil
  | Bool Bool
  deriving (Typeable)

instance Show LispVal where
  show = T.unpack . showVal

data IFunc = IFunc {fn :: [LispVal] -> Eval LispVal}

type EnvCtx = Map.Map T.Text LispVal

newtype Eval a = Eval {unEval :: ReaderT EnvCtx IO a}
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadReader EnvCtx,
      MonadIO
    )

showVal :: LispVal -> T.Text
showVal val =
  case val of
    Atom atom -> atom
    String str -> T.concat ["\"", str, "\""]
    Number num -> T.pack $ show num
    Bool True -> "#t"
    Bool False -> "#f"
    Nil -> "Nil"
    List contents -> T.concat $ ["(", T.unwords $ showVal <$> contents, ")"]
    Fun _ -> "(internal function)"
    Lambda _ _ -> "(lambda function)"
