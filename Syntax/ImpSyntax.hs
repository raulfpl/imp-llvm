module Syntax.ImpSyntax where

import Text.PrettyPrint

-- this module defines the Abstract syntax tree of IMP programs.

-- a program is just a list of commands

data Program = Program { 
		vars :: [VarDef], 
		body :: [Com] 
	       } deriving (Eq, Ord)

-- variable definition

data VarDef = VarDef Ty Variable
	      deriving (Eq, Ord)

-- types

data Ty = TyInt         -- integers
	| TyBool        -- booleans
	| TyString      -- string literals
	| TyVar Int     -- type variables. used in type inference only
	  deriving (Eq, Ord)

type Block = [Com]

-- a variable is just a string

type Variable = String


-- the definition of arithmetic expressions

data AExp = Const Int      -- a integer constant
          | Var Variable   -- a variable
          | AExp :+: AExp  -- summing two expressions 
          | AExp :-: AExp  -- subtracting two expressions
          | AExp :*: AExp  -- multiplying two expressions
	  | AExp :/: AExp  -- dividing two expressions
          deriving (Eq, Ord, Show)

-- definition of expressions

data BExp = BConst Bool      -- boolean constants
          | BExp :=: BExp    -- arithmetic equality
          | BExp :<=: BExp   -- arithmetic less-than-equal
          | Not BExp         -- negation
          | BExp :&: BExp    -- conjunction
          | BExp :|: BExp    -- disjunction
          | BNum AExp        -- arithmetic expression
          | BStr String      -- string literal
          deriving (Eq, Ord, Show)

-- commands of imp

data Com = Skip                     -- do nothing command
	 | Print BExp               -- print command
	 | Read Variable            -- read command
         | Assign Variable BExp     -- assignment
         | If BExp [Com] [Com]      -- conditional execution
         | While BExp [Com]         -- while loop
         deriving (Eq, Ord, Show)

-- pretty printing

class PPrint a where
   pprint :: a -> Doc

instance PPrint Com where
   pprint Skip = text "skip"
   pprint (Print e) = text "print" <> parens (pprint e)
   pprint (Read v) = text "read" <> parens (text v)
   pprint (Assign v e) = text v <+> text ":=" <+> pprint e
   pprint (If e cs cs') = text "if" <+> pprint e 
                                    <+> pList 3 cs
                                    <> nl <> text "else" 
                                    <+> pList 3 cs'
   pprint (While b cs) = text "while" <+> pprint b <+> 
                            pList 3 cs

instance PPrint Ty where
   pprint TyInt = text "int"
   pprint TyBool = text "bool"
   pprint TyString = text "string"
   pprint (TyVar n) = text "x_" <> int n

instance PPrint VarDef where
   pprint (VarDef t v) = pprint t <+> text v

instance PPrint AExp where
   pprint (Const n) = int n
   pprint (Var v)   = text v
   pprint (e :+: e') = pprint e <+> char '+' <+> pprint e'
   pprint (e :-: e') = pprint e <+> char '-' <+> pprint e'
   pprint (e :*: e') = pprint e <+> char '*' <+> pprint e'
   pprint (e :/: e') = pprint e <+> char '/' <+> pprint e'

instance PPrint BExp where
   pprint (BConst b) = bool b
   pprint (e :=: e') = pprint e <+> char '=' <+> pprint e' 
   pprint (e :<=: e') = pprint e <+> text "<=" <+> pprint e'
   pprint (e :&: e') = pprint e <+> char '&' <+> pprint e'
   pprint (e :|: e') = pprint e <+> char '|' <+> pprint e'
   pprint (Not e) = text "not" <> parens (pprint e)
   pprint (BNum e) = pprint e
   pprint (BStr s) = text s

-- auxiliar functions

nl :: Doc
nl = char '\n'

pList :: Int -> [Com] -> Doc
pList n = braces . nest n . hcat . punctuate nl . map pprint

bool :: Bool -> Doc
bool True = text "true"
bool False = text "false"
