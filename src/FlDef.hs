module FlDef where

data PrefixOp = Not | Neg
data InfixOp = Plus | Minus | Mul | Div | Eq | NotEq | Ls | Gt | Lse | Gte | And | Or | Concat | Cons | Index

newtype LowerVar = LowerVar String
newtype UpperVar = UpperVar String

data Pat
    = PatVar LowerVar
    | PatInt Int
    | PatChar Char
    | PatBool Bool
    | PatStr String
    | PatNil
    | PatList [Pat]
    | PatTuple [Pat]
    | PatCtor UpperVar [Pat]
data Type
    = TyVar LowerVar
    | TyInt
    | TyChar
    | TyBool
    | TyList Type
    | TyTuple [Type]
    | TyFunc [Type]

data QualCl = Guard Exp | Gen Pat Exp
data CaseCl = CaseCl {pat :: Pat, guards :: [GuardCl]}
data FuncCl = FuncCl {args :: [Pat], guards :: [GuardCl]}
data GuardCl = GuardCl (Maybe Exp) Exp
data Exp
    = ExpVar LowerVar
    | ExpInt Int
    | ExpChar Char
    | ExpBool Bool
    | ExpStr String
    | ExpList [Exp]
    | ExpTuple [Exp]
    | ExpListComp Exp [QualCl]
    | ExpCase {exp :: Exp, cases :: [CaseCl], defs :: [Def]}
    | ExpFunc {name :: LowerVar, clauses :: [FuncCl]}
    | ExpFuncApp {func :: Exp, arg :: Exp}
    | ExpPrefix PrefixOp Exp
    | ExpInfix Exp InfixOp Exp
    | ExpAdt UpperVar [Exp]

data Def
    = DefType LowerVar Type
    | DefAdt {name :: UpperVar, tyVars :: [Type], ctors :: [(UpperVar, [Type])]}
    | DefVar Pat Exp
data Prog = Prog [Def] Exp
