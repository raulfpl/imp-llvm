module Tc.ImpTc where

-- type inference / checking for IMP language

import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import qualified Data.Map as Map

import Syntax.ImpSyntax

-- typing context definition

type Context = Map.Map Variable Ty


-- type inference / checking monad definition

data TcEnv = TcEnv {
		gamma :: Context,
		fresh :: Int
	     } deriving (Eq, Ord)

emptyEnv :: TcEnv
emptyEnv = TcEnv Map.empty 0

type TcM a = (StateT TcEnv IO) a


-- Gamma related functions

lookupTy :: Variable -> TcM Ty
lookupTy v = do 
		ty <- gets (Map.lookup v . gamma)
		case ty of
		   Just t -> return t
		   Nothing -> do
				v' <- newTyVar
				modify incfresh
				ins v v'
				return v'

newTyVar :: TcM Ty
newTyVar = do 
	     v <- gets fresh
	     modify incfresh
	     return (TyVar v)

incfresh :: TcEnv -> TcEnv
incfresh t = t{fresh = (fresh t) + 1}

ins :: Variable -> Ty -> TcM ()
ins v t = modify (ins v t)
	  where
	    ins v t = \env -> env{gamma = Map.insert v t (gamma env)}

tc :: Program -> IO ()
tc p = evalStateT (tcProg (body p)) env'
	   where
		  env'' = foldr (\(VarDef t v) ac -> Map.insert v t ac) Map.empty (vars p)
		  env' = TcEnv env'' 0

-- type checking a program body

tcProg :: [Com] -> TcM ()
tcProg = mapM_ tcCom 

-- type checking for commands

tcCom :: Com -> TcM ()
tcCom Skip = return ()
tcCom (Print e) = do 
		    ty <- tcBExp e 
		    if ty `elem` [TyInt, TyBool, TyString] then return ()
			else fail ("type error on" ++ (show (Print e)))
tcCom (Read x) = return ()
tcCom (Assign v e) = do
			ty  <- lookupTy v
			ty' <- tcBExp e
			check ty ty'
			return ()
tcCom (If b c1 c2) = do
			ty <- tcBExp b
			check ty TyBool
			mapM_ tcCom c1
			mapM_ tcCom c2
tcCom (While b c) = do
			ty <- tcBExp b
			check ty TyBool
			mapM_ tcCom c

-- type checking for expressions

tcBExp :: BExp -> TcM Ty
tcBExp (BConst _) = return TyBool
tcBExp (e1 :=: e2) = binaryOpCheck e1 e2 tcBExp TyInt TyInt TyBool
tcBExp (e1 :<=: e2) = binaryOpCheck e1 e2 tcBExp TyInt TyInt TyBool
tcBExp (Not e) = do
		   t <- tcBExp e
		   check t TyBool
		   return TyBool
tcBExp (e1 :&: e2) = binaryOpCheck e1 e2 tcBExp TyBool TyBool TyBool
tcBExp (e1 :|: e2) = binaryOpCheck e1 e2 tcBExp TyBool TyBool TyBool
tcBExp (BNum e) = do
		    t <- tcAExp e
		    check t TyInt
		    return TyInt
tcBExp (BStr _) = return TyString

tcAExp :: AExp -> TcM Ty
tcAExp (Const _) = return TyInt
tcAExp (Var v) = lookupTy v
tcAExp (e1 :+: e2) = binaryOpCheck e1 e2 tcAExp TyInt TyInt TyInt
tcAExp (e1 :-: e2) = binaryOpCheck e1 e2 tcAExp TyInt TyInt TyInt
tcAExp (e1 :*: e2) = binaryOpCheck e1 e2 tcAExp TyInt TyInt TyInt
tcAExp (e1 :/: e2) = binaryOpCheck e1 e2 tcAExp TyInt TyInt TyInt
					

binaryOpCheck :: a -> a -> (a -> TcM Ty) -> Ty -> Ty -> Ty -> TcM Ty
binaryOpCheck e1 e2 tc t1 t2 tr
                    = do
			t1' <- tc e1
			t2' <- tc e2
			check t1 t1'
			check t2 t2'
			return tr

-- checking types

check :: Ty -> Ty -> TcM ()
check t1 t2 = if t1 == t2 then return ()
		else fail (concat ["type error:", show t1, "\n", show t2])
