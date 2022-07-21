module FlDef where

data PrefixOp = Not | Neg deriving (Eq, Show)

data InfixOp = Plus | Minus | Mul | Div | Eq | NotEq | Ls | Gt | Lse | Gte | And | Or | Concat | Cons | Index deriving (Eq, Show)

newtype LowerVar = LowerVar String deriving (Eq, Show)

newtype UpperVar = UpperVar String deriving (Eq, Show)

data Ctor a = Ctor UpperVar [a] deriving (Eq, Show)

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
    = TyVar LowerVar
    | TyInt
    | TyChar
    | TyBool
    | TyStr
    | TyList Type
    | TyTuple [Type]
    | TyFunc [Type]
    deriving (Eq, Show)

data QualCl = Guard Exp | Gen Pat Exp deriving (Eq, Show)

data CaseCl = CaseCl Pat [GuardCl] deriving (Eq, Show)

data FuncCl = FuncCl [Pat] [GuardCl] deriving (Eq, Show)

data GuardCl = GuardCl (Maybe Exp) Exp deriving (Eq, Show)

data Exp
    = ExpVar LowerVar
    | ExpInt Int
    | ExpChar Char
    | ExpBool Bool
    | ExpStr String
    | ExpList [Exp]
    | ExpTuple [Exp]
    | ExpListComp Exp [QualCl]
    | ExpCase Exp [CaseCl] [Def]
    | ExpFunc LowerVar [FuncCl]
    | ExpFuncApp Exp Exp
    | ExpPrefix PrefixOp Exp
    | ExpInfix Exp InfixOp Exp
    | ExpAdt (Ctor Exp)
    deriving (Eq, Show)

data Def
    = DefType LowerVar Type
    | DefAdt UpperVar [Type] [Ctor Type]
    | DefVar Pat Exp
    deriving (Eq, Show)

data Prog = Prog [Def] Exp deriving (Eq, Show)
