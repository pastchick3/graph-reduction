module ElcTranslator (translateElc) where

import  qualified ElcDef as Elc
import  qualified FlDef as Fl

translateElc :: Fl.Prog -> Either String Elc.Exp
translateElc flProg = Left "TODO"