module ElcDef where

import FlDef (InfixOp, LowerVar, PrefixOp, UpperVar)

data Exp
    = ExpVar LowerVar
    | ExpInt Int
    | ExpChar Char
    | ExpBool Bool
    | ExpStr String
    | ExpNil
    | ExpCons Exp Exp
    | ExpTuple2 Exp Exp
    | ExpTuple3 Exp Exp Exp
    | ExpTuple4 Exp Exp Exp Exp
    | ExpAdt UpperVar [Exp]
    | ExpPrefix PrefixOp Exp
    | ExpInfix InfixOp Exp Exp
    | ExpApp Exp Exp
    | ExpAbs Pat Exp
    | ExpLet Pat Exp Exp
    | ExpLetRec [(Pat, Exp)] Exp
    | ExpAppend Exp Exp
    | ExpCase Exp [(Pat, Exp)]
    deriving (Eq, Show)

data Pat
    = PatVar LowerVar
    | PatInt Int
    | PatChar Char
    | PatBool Bool
    | PatStr String
    | PatNil
    | PatCons Pat Pat
    | PatTuple2 Pat Pat
    | PatTuple3 Pat Pat Pat
    | PatTuple4 Pat Pat Pat Pat
    | PatAdt UpperVar [Pat]
    deriving (Eq, Show)
