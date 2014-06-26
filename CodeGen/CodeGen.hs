{-# LANGUAGE TypeFamilies #-}

module CodeGen.CodeGen where

import Control.Monad.State
import Control.Monad.Identity

import Text.PrettyPrint

import Syntax.ImpSyntax

type GenM a = (StateT Int Identity) a

class CodeGen a where
  type Result a
  gen :: a -> GenM (Result a)

instance CodeGen VarDef where
  type Result VarDef = Doc
  gen (VarDef ty v) = return $ text ("@" ++ v ++ " = common global i32 0, align 4")

instance CodeGen Program where
  type Result Program = Doc
  gen (Program vs cs) = do
                          x <-  liftM (hcat . punctuate nl) (mapM gen vs)
                          x' <- liftM (hcat . punctuate nl) (mapM gen cs)
                          return (hcat $ punctuate nl [header, x, x', end])

instance CodeGen Com where
  type Result Com = Doc
  gen Skip = return $ text ""

instance CodeGen AExp where
  type Result AExp = (Doc,Int) -- this Int holds, the variable that has the "result"
  gen (Const n) = do
                   v <- fresh
                   return (text $ concat ["%",show v," = add i32", " 0, ", show n], v)
  gen (Var v) = do
                  v' <- fresh
                  return (text $ concat ["%", show v', " = ", "load i32* @",v,",align 4"], v')
  gen (e :+: e') = genBinExp e e' "add"
  gen (e :*: e') = genBinExp e e' "mul"
  gen (e :-: e') = genBinExp e e' "sub"
  gen (e :/: e') = genBinExp e e' "sdiv"

instance CodeGen BExp where
  type Result BExp = (Doc,Int)
  gen (BConst b) = do
                     v <- fresh
                     let f True  = 1
                         f False = 0
                     return (text $ concat ["%",show v," = add i32", " 0, ", show (f b)], v)
  gen (e :|: e') = do
     (c,v) <- gen e
     (c',v') <- gen e'
     [l1, r1, l2, r2, l3, r3, r4] <- mapM fresh [1..6]
     return (text $ concat ["br label %", show l1, "\n",
                           "; <label>:", show l1, "\n",
                           "%", show r1, " = icmp ne i32 %", show v, ", 0\n",
                           "br i1 %", show r1, " label %", show l2, ", label %", show l3, "\n",
                           "\n",
                           "; <label>:", show l2, "\n",
                           "%", show r2, " = icmp ne i32 %", show v', " ,0\n",
                           "br label %", show l3, "\n",
                           "\n",
                           "; <label>:", show l3, "\n",
                           "%", show r3, " = phi il [ false,%", show l1, " ], [ %", show r2, ", %", show l2, "]\n",
                           "%", show r4, " = zext i1 %", show r3, " to i32\n"] ,r4)
  gen (e :&: e') = do
     (c,v) <- gen e
     (c',v') <- gen e'
     [l1, r1, l2, r2, l3, r3, r4] <- mapM fresh [1..6]
     return (text $ concat ["br label %", show l1, "\n",
                           "; <label>:", show l1, "\n",
                           "%", show r1, " = icmp ne i32 %", show v, ", 0\n",
                           "br i1 %", show r1, " label %", show l3, ", label %", show l2, "\n",
                           "\n",
                           "; <label>:", show l2, "\n",
                           "%", show r2, " = icmp ne i32 %", show v', " ,0\n",
                           "br label %", show l3, "\n",
                           "\n",
                           "; <label>:", show l3, "\n",
                           "%", show r3, " = phi il [ true,%", show l1, " ], [ %", show r2, ", %", show l2, "]\n",
                           "%", show r4, " = zext i1 %", show r3, " to i32\n"] ,r4)

genBinExp :: AExp -> AExp -> String -> GenM (Result AExp)
genBinExp e e' op = do
                      (c,v) <- gen e
                      (c',v') <- gen e'
                      r <- fresh
                      return (text $ concat ["%",show r," = ", op, " nsw i32 %",
                                                show v, ", %", show v'], r)

fresh :: GenM Int
fresh = do
          v <- get
          put  (v + 1)
          return v

header :: Doc
header = text "define int @main() #0"

end :: Doc
end = text "ret 0"


