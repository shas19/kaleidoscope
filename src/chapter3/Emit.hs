{-# LANGUAGE OverloadedStrings #-}

module Emit where

import LLVM.Module
import LLVM.Context

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP

import qualified Data.ByteString as BS
import Data.Word
import Data.Int
import Control.Monad.Except
import Control.Applicative
import qualified Data.Map as Map

import Codegen
import qualified Syntax as S

toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.mkName x))

toType :: [S.Expr] -> [AST.Type]
toType = map (\x -> double)

codegenTop :: S.Expr -> LLVM ()
codegenTop (S.Function name args body) = do
  define double name fnargs bls
  where
    fnargs = toSig args
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM args $ \a -> do
        var <- alloca double
        store var (local (AST.mkName a))
        assign a var
        --assign a (local (AST.mkName a))
      cgen body >>= ret

codegenTop (S.Extern name args) = do
  external double name fnargs
  where fnargs = toSig args

codegenTop exp = do
  define double "main" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen exp >>= ret

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = do
  test <- fcmp FP.ULT a b
  uitofp double test

binops = Map.fromList [
      ("+", fadd)
    , ("-", fsub)
    , ("*", fmul)
    , ("/", fdiv)
    , ("<", lt)
  ]


cgen :: S.Expr -> Codegen AST.Operand
cgen (S.UnaryOp op a) = do
  cgen $ S.Call ("unary" ++ op) [a]
cgen (S.BinaryOp "=" (S.Var var) val) = do
  a <- getvar var
  cval <- cgen val
  store a cval
  return cval
cgen (S.BinaryOp op a b) = do
  case Map.lookup op binops of
    Just f  -> do
      ca <- cgen a
      cb <- cgen b
      f ca cb
    Nothing -> error "No such operator"
cgen (S.Var x) = getvar x >>= load
cgen (S.Float n) = return $ cons $ C.Float (F.Double n)
cgen (S.Call fn args) = do
  largs <- mapM cgen args
  call (global (AST.mkName fn) (toType args)) largs

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

-- So withContext allows using the state Context which allows multithreaded compiling
-- withModuleFromAST extracts particular modules from the context 
codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen mod fns = withContext $ \context ->
  withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    BS.putStrLn llstr
    return newast
  where
    modn    = mapM codegenTop fns
    newast  = runLLVM mod modn
