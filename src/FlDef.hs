module FlDef where

data PrefixOp = Not | Neg deriving (Eq, Show)

data InfixOp = Plus | Minus | Mul | Div | Eq | NotEq | Lse | Ls | Gte | Gt | And | Or | Concat | Cons | Index deriving (Eq, Show)

newtype LowerVar = LowerVar String deriving (Eq, Show)

newtype UpperVar = UpperVar String deriving (Eq, Show)

data Ctor a = Ctor UpperVar [a] deriving (Eq, Show)

newtype Prog = Prog [Def] deriving (Eq, Show)

data Def
    = DefAdt UpperVar [Type] [Ctor Type]
    | DefType LowerVar Type
    | DefVar Pat Exp
    | DefFunc LowerVar [Pat] [GuardCl] [Def]
    deriving (Eq, Show)

data Exp
    = ExpGroup Exp
    | ExpVar LowerVar
    | ExpInt Int
    | ExpChar Char
    | ExpBool Bool
    | ExpStr String
    | ExpList [Exp]
    | ExpTuple [Exp]
    | ExpAdt (Ctor Exp)
    | ExpListComp Exp [QualCl]
    | ExpCase Exp [CaseCl]
    | ExpFuncApp Exp Exp
    | ExpPrefix PrefixOp Exp
    | ExpInfix InfixOp Exp Exp
    deriving (Eq, Show)

data QualCl = Gen Pat Exp | Guard Exp deriving (Eq, Show)

data CaseCl = CaseCl Pat [GuardCl] deriving (Eq, Show)

data GuardCl = GuardCl (Maybe Exp) Exp deriving (Eq, Show)

data Pat
    = PatVar LowerVar
    | PatInt Int
    | PatChar Char
    | PatBool Bool
    | PatStr String
    | PatList [Pat]
    | PatTuple [Pat]
    | PatCtor (Ctor Pat)
    deriving (Eq, Show)

data Type
    = TyGroup Type
    | TyVar LowerVar
    | TyInt
    | TyChar
    | TyBool
    | TyStr
    | TyList Type
    | TyTuple [Type]
    | TyFunc [Type]
    deriving (Eq, Show)
