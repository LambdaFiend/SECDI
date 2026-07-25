module Display where

import           Data.List
import           Lexer
import           Syntax

showTerm'' :: Environment -> TermNode -> String
showTerm'' ctx t =
  case (getTm t, showTerm ctx t) of
    (TmPair _ _, s) -> s
    (TmUnit, s)     -> s
    (_, s)          -> removeOuterParens s

showTerm' :: TermNode -> String
showTerm' t =
  case (getTm t, showTerm [] t) of
    (TmPair _ _, s) -> s
    (TmUnit, s)     -> s
    (_, s)          -> removeOuterParens s

showTerm :: Environment -> TermNode -> String
showTerm ctx t =
  case getTm t of
    TmInt n -> show n
    TmBool n -> if n then "true" else "false"
    -- showTerm ctx v instead? I'm worried about possible stack overflows
    TmVar x -> case lookup x ctx of Nothing -> x; Just (TermNode _ (TmTyingTheKnot _ _ _)) -> x; Just (TermNode _ TmEmptyKnot) -> x; Just v -> showTerm [] v
    TmAbs x t1 -> "(" ++ "λ" ++ x ++ "." ++ showTerm ctx t1 ++ ")"
    TmApp t1 t2 -> "(" ++ showTerm ctx t1 ++ " " ++ showTerm ctx t2 ++ ")"
    TmIf t1 t2 t3 -> "(" ++ "if " ++ showTerm ctx t1 ++ " then " ++ showTerm ctx t2 ++ " else " ++ showTerm ctx t3 ++ ")"
    TmAnd t1 t2 -> "(" ++ showTerm ctx t1 ++ " && " ++ showTerm ctx t2 ++ ")"
    TmOr t1 t2 -> "(" ++ showTerm ctx t1 ++ " || " ++ showTerm ctx t2 ++ ")"
    TmEq t1 t2 -> "(" ++ showTerm ctx t1 ++ " == " ++ showTerm ctx t2 ++ ")"
    TmNE t1 t2 -> "(" ++ showTerm ctx t1 ++ " /= " ++ showTerm ctx t2 ++ ")"
    TmLT t1 t2 -> "(" ++ showTerm ctx t1 ++ " < " ++ showTerm ctx t2 ++ ")"
    TmGT t1 t2 -> "(" ++ showTerm ctx t1 ++ " > " ++ showTerm ctx t2 ++ ")"
    TmLE t1 t2 -> "(" ++ showTerm ctx t1 ++ " <= " ++ showTerm ctx t2 ++ ")"
    TmGE t1 t2 -> "(" ++ showTerm ctx t1 ++ " >= " ++ showTerm ctx t2 ++ ")"
    TmPlus t1 t2 -> "(" ++ showTerm ctx t1 ++ " + " ++ showTerm ctx t2 ++ ")"
    TmSub t1 t2 -> "(" ++ showTerm ctx t1 ++ " - " ++ showTerm ctx t2 ++ ")"
    TmMult t1 t2 -> "(" ++ showTerm ctx t1 ++ " * " ++ showTerm ctx t2 ++ ")"
    TmDiv t1 t2 -> "(" ++ showTerm ctx t1 ++ " / " ++ showTerm ctx t2 ++ ")"
    TmPair t1 t2 -> "(" ++ showTerm ctx t1 ++ ", " ++ showTerm ctx t2 ++ ")"
    TmFix -> "Y"
    TmNot -> "not"
    TmFst -> "fst"
    TmSnd -> "snd"
    TmTyingTheKnot _ _ _ -> "·"
    TmEmptyKnot -> "⊥"
    TmClosure x t1 e -> "(" ++ "λ" ++ x ++ "." ++ showTerm e t1 ++ ")"
    TmLetrec ts t1 -> "(" ++ "letrec " ++ intercalate " and " (map (\(x, y) -> x ++ " = " ++ showTerm ctx y) ts) ++ " in " ++ showTerm ctx t1 ++ ")"
    TmUnit -> "()"
    TmError e -> "#" ++ e ++ "#"

showFileInfo :: FileInfo -> String
showFileInfo (AlexPn p l c) =
  "\n"
    ++ "Absolute Offset: "
    ++ show p
    ++ "\n"
    ++ "Line: "
    ++ show l
    ++ "\n"
    ++ "Column: "
    ++ show c

removeOuterParens :: String -> String
removeOuterParens xs
  | length xs >= 2 =
      let xs' = reverse $ getTail xs
       in if getHead xs == '(' && getHead xs' == ')'
            then reverse $ getTail xs'
            else xs
  | otherwise = xs
  where
    getHead = (\ws -> case ws of (y : _) -> y; _ -> '\0')
    getTail = (\ws -> case ws of (_ : ys) -> ys; _ -> [])

showSECD :: SECD -> String
showSECD (s, e, c, d) = "(" ++ showStack s ++ ", " ++ showEnvironment e ++ ", " ++ showControl e c ++ ", " ++ showDump d ++ ")"

showStack :: Stack -> String
showStack EmptyStack = "•"
showStack (ClosureStack x t e s) = "Clo(" ++ x ++ ", " ++ showTerm' t ++ ", " ++ showEnvironment e ++ ")" ++ "; " ++ showStack s
showStack (ValueStack t s) = showTerm' t ++ "; " ++ showStack s

showEnvironment :: Environment -> String
showEnvironment [] = "•"
showEnvironment ((x, t) : e) = x ++ " = " ++ showTerm' t ++ "; " ++ showEnvironment e

showControl :: Environment -> Control -> String
showControl _ EmptyControl = "•"
showControl e (InstructionControl i c) = showInstruction e i ++ "; " ++ showControl e c
showControl e (TermControl t c) = showTerm' t ++ "; " ++ showControl e c

showDump :: Dump -> String
showDump EmptyDump = "•"
showDump (NonEmptyDump s e c d) = "(" ++ showStack s ++ ", " ++ showEnvironment e ++ ", " ++ showControl e c ++ ", " ++ showDump d ++ ")"

showInstruction :: Environment -> Instruction -> String
showInstruction e i =
  case i of
    InstrApp -> "App"
    InstrAnd t1 -> "And" ++ "(" ++ showTerm e t1 ++ ")"
    InstrOr t1 -> "Or" ++ "(" ++ showTerm e t1 ++ ")"
    InstrEq -> "Eq"
    InstrNE -> "NE"
    InstrLT -> "LT"
    InstrGT -> "GT"
    InstrLE -> "LE"
    InstrGE -> "GE"
    InstrPlus -> "Plus"
    InstrSub -> "Sub"
    InstrMult -> "Mult"
    InstrDiv -> "Div"
    InstrPair -> "Pair"
    InstrIf t1 t2 -> "If" ++ "(" ++ showTerm e t1 ++ ", " ++ showTerm e t2 ++ ")"
