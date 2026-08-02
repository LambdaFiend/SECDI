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
    TmClosure x c1 e ->
      case showPrettyControl e (reverse (toListControl c1)) of
        (x', []) -> "(" ++ "λ" ++ x ++ "." ++ x' ++ ")"
        (x', l) -> "#" ++ "Failed to show pretty Control at TmClosure, as the remainding list was not empty: " ++ "(" ++ x' ++ ", " ++ show l ++ ")" ++ " for the control " ++ showControl ctx c1 ++ " which was converted into the list " ++ show (reverse (toListControl c1)) ++ "#"
    TmLetrec ts t1 -> "(" ++ "letrec " ++ intercalate " and " (map (\(x, y) -> let (xs, y') = showLetrecArgs y in x ++ " " ++ intercalate " " xs ++ " = " ++ showTerm ctx y') ts) ++ " in " ++ showTerm ctx t1 ++ ")"
    TmUnit -> "()"
    TmControl c ->
      case showPrettyControl ctx (reverse (toListControl c)) of
        (x, []) -> x
        (x, l) -> "#" ++ "Failed to show pretty Control at TmControl, as the remainding list was not empty: " ++ "(" ++ x ++ ", " ++ show l ++ ")" ++ " for the control " ++ showControl ctx c ++ " which was converted into the list " ++ show (reverse (toListControl c)) ++ "#"
    TmError e -> "#" ++ e ++ "#"

toListControl :: Control -> [Instruction]
toListControl EmptyControl              = []
toListControl (TermControl _ cs)        = toListControl cs
toListControl (InstructionControl i cs) = i : toListControl cs

showPrettyControl :: Environment -> [Instruction] -> (String, [Instruction])
showPrettyControl _ [] = ("", [])
showPrettyControl ctx (InstrApp : i1 : is) =
  let (x1, is') = showPrettyControl ctx (i1 : is)
      (x2, is'') = showPrettyControl ctx is'
   in ("(" ++ x1 ++ " " ++ x2 ++ ")", is'')
showPrettyControl ctx (InstrPlus : i1 : is) =
  let (x1, is') = showPrettyControl ctx (i1 : is)
      (x2, is'') = showPrettyControl ctx is'
   in ("(" ++ x2 ++ " + " ++ x1 ++ ")", is'')
showPrettyControl ctx (InstrSub : i1 : is) =
  let (x1, is') = showPrettyControl ctx (i1 : is)
      (x2, is'') = showPrettyControl ctx is'
   in ("(" ++ x2 ++ " - " ++ x1 ++ ")", is'')
showPrettyControl ctx (InstrMult : i1 : is) =
  let (x1, is') = showPrettyControl ctx (i1 : is)
      (x2, is'') = showPrettyControl ctx is'
   in ("(" ++ x2 ++ " * " ++ x1 ++ ")", is'')
showPrettyControl ctx (InstrDiv : i1 : is) =
  let (x1, is') = showPrettyControl ctx (i1 : is)
      (x2, is'') = showPrettyControl ctx is'
   in ("(" ++ x2 ++ " / " ++ x1 ++ ")", is'')
showPrettyControl ctx (InstrLT : i1 : is) =
  let (x1, is') = showPrettyControl ctx (i1 : is)
      (x2, is'') = showPrettyControl ctx is'
   in ("(" ++ x2 ++ " < " ++ x1 ++ ")", is'')
showPrettyControl ctx (InstrGT : i1 : is) =
  let (x1, is') = showPrettyControl ctx (i1 : is)
      (x2, is'') = showPrettyControl ctx is'
   in ("(" ++ x2 ++ " > " ++ x1 ++ ")", is'')
showPrettyControl ctx (InstrLE : i1 : is) =
  let (x1, is') = showPrettyControl ctx (i1 : is)
      (x2, is'') = showPrettyControl ctx is'
   in ("(" ++ x2 ++ " <= " ++ x1 ++ ")", is'')
showPrettyControl ctx (InstrGE : i1 : is) =
  let (x1, is') = showPrettyControl ctx (i1 : is)
      (x2, is'') = showPrettyControl ctx is'
   in ("(" ++ x2 ++ " >= " ++ x1 ++ ")", is'')
showPrettyControl ctx (InstrPair : i1 : is) =
  let (x1, is') = showPrettyControl ctx (i1 : is)
      (x2, is'') = showPrettyControl ctx is'
   in ("(" ++ x2 ++ ", " ++ x1 ++ ")", is'')
showPrettyControl ctx (InstrIf c2 c3 : i1 : is) =
  let (x1, is') = showPrettyControl ctx (i1 : is)
      (x2, _) = showPrettyControl ctx (reverse (toListControl c2))
      (x3, _) = showPrettyControl ctx (reverse (toListControl c3))
   in ("(" ++ "if " ++ x1 ++ " then " ++ x2 ++ " else " ++ x3 ++ ")", is')
showPrettyControl ctx (InstrAnd c1 : is) =
  let (x1, _) = showPrettyControl ctx (reverse (toListControl c1))
      (x2, is') = showPrettyControl ctx is
   in ("(" ++ x2 ++ " && " ++ x1 ++ ")", is')
showPrettyControl ctx (InstrOr c1 : is) =
  let (x1, _) = showPrettyControl ctx (reverse (toListControl c1))
      (x2, is') = showPrettyControl ctx is
   in ("(" ++ x2 ++ " || " ++ x1 ++ ")", is')
showPrettyControl ctx (InstrVar x : is) = (case lookup x ctx of Nothing -> x; Just (TermNode _ (TmTyingTheKnot _ _ _)) -> x; Just (TermNode _ TmEmptyKnot) -> x; Just v -> showTerm [] v, is)
showPrettyControl _ (InstrConstInt n : is) = (show n, is)
showPrettyControl _ (InstrConstBool b : is) = (if b then "true" else "false", is)
showPrettyControl _ (InstrConstUnit : is) = ("()", is)
showPrettyControl _ (InstrConstNot : is) = ("not", is)
showPrettyControl _ (InstrConstFst : is) = ("fst", is)
showPrettyControl _ (InstrConstSnd : is) = ("snd", is)
showPrettyControl ctx (InstrClosure x c1 : is) =
  let (x1, _) = showPrettyControl ctx (reverse (toListControl c1))
   in ("(" ++ "λ" ++ x ++ "." ++ x1 ++ ")", is)
showPrettyControl ctx (InstrKnot f x c1 : is) =
  let (x1, _) = showPrettyControl ctx (reverse (toListControl c1))
   in ("(" ++ "λ" ++ f ++ "." ++ "λ" ++ x ++ "." ++ x1 ++ ")", is)
showPrettyControl ctx (InstrLetrec xtx c1 : is) =
  let (x1, _) = showPrettyControl ctx (reverse (toListControl c1))
   in ("(" ++ "letrec " ++ intercalate " and " (map (showPrettyControlLetrec ctx) xtx) ++ " in " ++ x1 ++ ")", is)
showPrettyControl _ is = ("#During show pretty Control, there was a pattern match failure, somehow. Considering that the term managed to compile, this shouldn't have happened#", is)

showPrettyControlLetrec :: Environment -> (Name, (Control, Maybe Name)) -> String
showPrettyControlLetrec ctx (x1, (c2, Nothing)) =
  let (xs, c2') = showLetrecArgsControl (reverse (toListControl c2))
      (x2, _) = showPrettyControl ctx c2'
   in x1 ++ " " ++ intercalate " " xs ++ " = " ++ x2
showPrettyControlLetrec ctx (x1, (c2, Just x3)) =
  let (xs, c2') = showLetrecArgsControl (reverse (toListControl c2))
      (x2, _) = showPrettyControl ctx c2'
   in x1 ++ " " ++ x3 ++ " " ++ intercalate " " xs ++ " = " ++ x2

showLetrecArgs :: TermNode -> ([Name], TermNode)
showLetrecArgs (TermNode _ (TmAbs x t1)) =
  let (s, t) = showLetrecArgs t1
   in (x : s, t)
showLetrecArgs t1 = ([], t1)

showLetrecArgsControl :: [Instruction] -> ([Name], [Instruction])
showLetrecArgsControl [InstrClosure x c1] =
  let (s, c1') = showLetrecArgsControl (reverse (toListControl c1))
   in (x : s, c1')
showLetrecArgsControl is = ([], is)

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
showStack (ClosureStack x t e s) = "Clo(" ++ x ++ ", " ++ showControl e t ++ ", " ++ showEnvironment e ++ ")" ++ "; " ++ showStack s
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
    InstrAnd t1 -> "And" ++ "(" ++ showControl e t1 ++ ")"
    InstrOr t1 -> "Or" ++ "(" ++ showControl e t1 ++ ")"
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
    InstrIf c1 c2 -> "If" ++ "(" ++ showControl e c1 ++ ", " ++ showControl e c2 ++ ")"
    InstrConstInt n -> "Const" ++ "(" ++ show n ++ ")"
    InstrConstBool b -> "Const" ++ "(" ++ (if b then "true" else "false") ++ ")"
    InstrConstUnit -> "Const(())"
    InstrConstFst -> "Const(fst)"
    InstrConstSnd -> "Const(snd)"
    InstrConstNot -> "Const(not)"
    InstrVar x -> "Var" ++ "(" ++ x ++ ")"
    InstrClosure x c -> "InstrClosure" ++ "(" ++ x ++ ", " ++ showControl e c ++ ")"
    InstrKnot f x c -> "InstrKnot" ++ "(" ++ f ++ ", " ++ x ++ ", " ++ showControl e c ++ ")"
    InstrLetrec xtx c -> "InstrLetrec" ++ "(" ++ "[" ++ intercalate ", " (map (\(x1, (c2, x3)) -> "(" ++ x1 ++ ", " ++ showControl e c2 ++ ", " ++ show x3 ++ ")") xtx) ++ "]" ++ ", " ++ showControl e c ++ ")"
