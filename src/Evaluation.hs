module Evaluation where

import           Display
import           Syntax

compile :: TermNode -> Either String Control
compile t =
  case getTm t of
    TmApp (TermNode _ TmFix) (TermNode _ (TmAbs f (TermNode _ (TmAbs x t1)))) ->
      case compile t1 of
        Left e  -> Left e
        Right c -> Right (InstructionControl (InstrKnot f x c) EmptyControl)
    TmInt n -> Right (InstructionControl (InstrConstInt n) EmptyControl)
    TmBool b -> Right (InstructionControl (InstrConstBool b) EmptyControl)
    TmUnit -> Right (InstructionControl InstrConstUnit EmptyControl)
    TmFst -> Right (InstructionControl InstrConstFst EmptyControl)
    TmSnd -> Right (InstructionControl InstrConstSnd EmptyControl)
    TmNot -> Right (InstructionControl InstrConstNot EmptyControl)
    TmVar x -> Right (InstructionControl (InstrVar x) EmptyControl)
    TmAbs x t1 ->
      case compile t1 of
        Left e  -> Left e
        Right c -> Right (InstructionControl (InstrClosure x c) EmptyControl)
    TmApp t1 t2 ->
      case (compile t1, compile t2) of
        (Left e1, Left e2) -> Left (e1 ++ "\n" ++ e2)
        (Left e1, _) -> Left e1
        (_, Left e2) -> Left e2
        (Right c1, Right c2) -> Right (c2 `appendControl` c1 `appendControl` InstructionControl InstrApp EmptyControl)
    TmIf t1 t2 t3 ->
      case (compile t1, compile t2, compile t3) of
        (Left e1, Left e2, Left e3) -> Left (e1 ++ "\n" ++ e2 ++ "\n" ++ e3)
        (Left e1, Left e2, _) -> Left (e1 ++ "\n" ++ e2)
        (Left e1, _, Left e3) -> Left (e1 ++ "\n" ++ e3)
        (_, Left e2, Left e3) -> Left (e2 ++ "\n" ++ e3)
        (Left e1, _, _) -> Left e1
        (_, Left e2, _) -> Left e2
        (_, _, Left e3) -> Left e3
        (Right c1, Right c2, Right c3) -> Right (c1 `appendControl` InstructionControl (InstrIf c2 c3) EmptyControl)
    TmPlus t1 t2 ->
      case (compile t1, compile t2) of
        (Left e1, Left e2) -> Left (e1 ++ "\n" ++ e2)
        (Left e1, _) -> Left e1
        (_, Left e2) -> Left e2
        (Right c1, Right c2) -> Right (c1 `appendControl` c2 `appendControl` InstructionControl InstrPlus EmptyControl)
    TmSub t1 t2 ->
      case (compile t1, compile t2) of
        (Left e1, Left e2) -> Left (e1 ++ "\n" ++ e2)
        (Left e1, _) -> Left e1
        (_, Left e2) -> Left e2
        (Right c1, Right c2) -> Right (c1 `appendControl` c2 `appendControl` InstructionControl InstrSub EmptyControl)
    TmMult t1 t2 ->
      case (compile t1, compile t2) of
        (Left e1, Left e2) -> Left (e1 ++ "\n" ++ e2)
        (Left e1, _) -> Left e1
        (_, Left e2) -> Left e2
        (Right c1, Right c2) -> Right (c1 `appendControl` c2 `appendControl` InstructionControl InstrMult EmptyControl)
    TmDiv t1 t2 ->
      case (compile t1, compile t2) of
        (Left e1, Left e2) -> Left (e1 ++ "\n" ++ e2)
        (Left e1, _) -> Left e1
        (_, Left e2) -> Left e2
        (Right c1, Right c2) -> Right (c1 `appendControl` c2 `appendControl` InstructionControl InstrDiv EmptyControl)
    TmAnd t1 t2 ->
      case (compile t1, compile t2) of
        (Left e1, Left e2) -> Left (e1 ++ "\n" ++ e2)
        (Left e1, _) -> Left e1
        (_, Left e2) -> Left e2
        (Right c1, Right c2) -> Right (c1 `appendControl` InstructionControl (InstrAnd c2) EmptyControl)
    TmOr t1 t2 ->
      case (compile t1, compile t2) of
        (Left e1, Left e2) -> Left (e1 ++ "\n" ++ e2)
        (Left e1, _) -> Left e1
        (_, Left e2) -> Left e2
        (Right c1, Right c2) -> Right (c1 `appendControl` InstructionControl (InstrOr c2) EmptyControl)
    TmEq t1 t2 ->
      case (compile t1, compile t2) of
        (Left e1, Left e2) -> Left (e1 ++ "\n" ++ e2)
        (Left e1, _) -> Left e1
        (_, Left e2) -> Left e2
        (Right c1, Right c2) -> Right (c1 `appendControl` c2 `appendControl` InstructionControl InstrEq EmptyControl)
    TmNE t1 t2 ->
      case (compile t1, compile t2) of
        (Left e1, Left e2) -> Left (e1 ++ "\n" ++ e2)
        (Left e1, _) -> Left e1
        (_, Left e2) -> Left e2
        (Right c1, Right c2) -> Right (c1 `appendControl` c2 `appendControl` InstructionControl InstrNE EmptyControl)
    TmLT t1 t2 ->
      case (compile t1, compile t2) of
        (Left e1, Left e2) -> Left (e1 ++ "\n" ++ e2)
        (Left e1, _) -> Left e1
        (_, Left e2) -> Left e2
        (Right c1, Right c2) -> Right (c1 `appendControl` c2 `appendControl` InstructionControl InstrLT EmptyControl)
    TmGT t1 t2 ->
      case (compile t1, compile t2) of
        (Left e1, Left e2) -> Left (e1 ++ "\n" ++ e2)
        (Left e1, _) -> Left e1
        (_, Left e2) -> Left e2
        (Right c1, Right c2) -> Right (c1 `appendControl` c2 `appendControl` InstructionControl InstrGT EmptyControl)
    TmLE t1 t2 ->
      case (compile t1, compile t2) of
        (Left e1, Left e2) -> Left (e1 ++ "\n" ++ e2)
        (Left e1, _) -> Left e1
        (_, Left e2) -> Left e2
        (Right c1, Right c2) -> Right (c1 `appendControl` c2 `appendControl` InstructionControl InstrLE EmptyControl)
    TmGE t1 t2 ->
      case (compile t1, compile t2) of
        (Left e1, Left e2) -> Left (e1 ++ "\n" ++ e2)
        (Left e1, _) -> Left e1
        (_, Left e2) -> Left e2
        (Right c1, Right c2) -> Right (c1 `appendControl` c2 `appendControl` InstructionControl InstrGE EmptyControl)
    TmPair t1 t2 ->
      case (compile t1, compile t2) of
        (Left e1, Left e2) -> Left (e1 ++ "\n" ++ e2)
        (Left e1, _) -> Left e1
        (_, Left e2) -> Left e2
        (Right c1, Right c2) -> Right (c1 `appendControl` c2 `appendControl` InstructionControl InstrPair EmptyControl)
    TmFix -> compile yCombSubst
    TmLetrec xts t1 ->
      case (compileLetrec xts, compile t1) of
        (Left e1, Left e2) -> Left (e1 ++ "\n" ++ e2)
        (Left e1, _) -> Left e1
        (_, Left e2) -> Left e2
        (Right xcs, Right c) -> Right (InstructionControl (InstrLetrec xcs c) EmptyControl)
    TmError e -> Left e
    _ -> Left ("No rule applies: " ++ showTerm' t)

compileLetrec :: [(Name, TermNode)] -> Either String [(Name, (Control, Maybe Name))]
compileLetrec [] = Right []
compileLetrec ((x, t) : xts) =
  case getTm t of
    TmAbs x' t1 ->
      case (compile t1, compileLetrec xts) of
        (Left e1, Left e2)   -> Left (e1 ++ "\n" ++ e2)
        (Left e1, _)         -> Left e1
        (_, Left e2)         -> Left e2
        (Right c, Right xcs) -> Right ((x, (c, Just x')) : xcs)
    _ ->
      case (compile t, compileLetrec xts) of
        (Left e1, Left e2)   -> Left (e1 ++ "\n" ++ e2)
        (Left e1, _)         -> Left e1
        (_, Left e2)         -> Left e2
        (Right c, Right xcs) -> Right ((x, (c, Nothing)) : xcs)

eval1 :: SECD -> Either String SECD
eval1 m =
  case m of
    (s, e, TermControl v@(TermNode _ TmNot) c, d) -> Right (ValueStack v s, e, c, d)
    (s, e, TermControl v@(TermNode _ TmFst) c, d) -> Right (ValueStack v s, e, c, d)
    (s, e, TermControl v@(TermNode _ TmSnd) c, d) -> Right (ValueStack v s, e, c, d)
    (s, e, TermControl v@(TermNode _ (TmInt _)) c, d) -> Right (ValueStack v s, e, c, d)
    (s, e, TermControl v@(TermNode _ (TmBool _)) c, d) -> Right (ValueStack v s, e, c, d)
    (s, e, TermControl v@(TermNode _ TmUnit) c, d) -> Right (ValueStack v s, e, c, d)
    (s, e, TermControl (TermNode _ (TmControl c')) c, d) -> Right (s, e, c' `appendControl` c, d)
    (s, e, InstructionControl InstrConstNot c, d) -> Right (ValueStack (TermNode noPos TmNot) s, e, c, d)
    (s, e, InstructionControl InstrConstFst c, d) -> Right (ValueStack (TermNode noPos TmFst) s, e, c, d)
    (s, e, InstructionControl InstrConstSnd c, d) -> Right (ValueStack (TermNode noPos TmSnd) s, e, c, d)
    (s, e, InstructionControl (InstrConstInt n) c, d) -> Right (ValueStack (TermNode noPos (TmInt n)) s, e, c, d)
    (s, e, InstructionControl (InstrConstBool b) c, d) -> Right (ValueStack (TermNode noPos (TmBool b)) s, e, c, d)
    (s, e, InstructionControl InstrConstUnit c, d) -> Right (ValueStack (TermNode noPos TmUnit) s, e, c, d)
    (s, e, InstructionControl (InstrVar x) c, d) ->
      case lookup x e of
        Just (TermNode _ (TmClosure x1 t1 e')) -> Right (ClosureStack x1 t1 e' s, e, c, d)
        Just (TermNode _ (TmTyingTheKnot x1 t1 e')) -> Right (ClosureStack x1 t1 e' s, e, c, d)
        Just r -> Right (ValueStack r s, e, c, d)
        Nothing -> Right (ValueStack (TermNode noPos (TmVar x)) s, e, c, d)
    (s, e, InstructionControl (InstrClosure x c1) c, d) -> Right (ClosureStack x c1 e s, e, c, d)
    (s, e, InstructionControl (InstrKnot f x c1) c, d) -> Right (ClosureStack x c1 ((f, TermNode noPos TmEmptyKnot) : e) s, e, c, d)
    (s, e, InstructionControl (InstrLetrec cs c1) c, d) -> Right (EmptyStack, letrecEnvKnot e (zip (map fst cs) (map letrecKnotControl (map snd cs))), c1, NonEmptyDump s e c d)
    (s, e, TermControl (TermNode _ (TmPair t1 t2)) c, d) -> Right (s, e, TermControl t1 (TermControl t2 (InstructionControl InstrPair c)), d)
    (s, e, TermControl (TermNode _ (TmAnd t1 t2)) c, d) -> Right (s, e, TermControl t1 (InstructionControl (InstrAnd (TermControl t2 EmptyControl)) c), d)
    (s, e, TermControl (TermNode _ (TmOr t1 t2)) c, d) -> Right (s, e, TermControl t1 (InstructionControl (InstrOr (TermControl t2 EmptyControl)) c), d)
    (s, e, TermControl (TermNode _ (TmEq t1 t2)) c, d) -> Right (s, e, TermControl t1 (TermControl t2 (InstructionControl InstrEq c)), d)
    (s, e, TermControl (TermNode _ (TmNE t1 t2)) c, d) -> Right (s, e, TermControl t1 (TermControl t2 (InstructionControl InstrNE c)), d)
    (s, e, TermControl (TermNode _ (TmLT t1 t2)) c, d) -> Right (s, e, TermControl t1 (TermControl t2 (InstructionControl InstrLT c)), d)
    (s, e, TermControl (TermNode _ (TmGT t1 t2)) c, d) -> Right (s, e, TermControl t1 (TermControl t2 (InstructionControl InstrGT c)), d)
    (s, e, TermControl (TermNode _ (TmLE t1 t2)) c, d) -> Right (s, e, TermControl t1 (TermControl t2 (InstructionControl InstrLE c)), d)
    (s, e, TermControl (TermNode _ (TmGE t1 t2)) c, d) -> Right (s, e, TermControl t1 (TermControl t2 (InstructionControl InstrGE c)), d)
    (s, e, TermControl (TermNode _ (TmPlus t1 t2)) c, d) -> Right (s, e, TermControl t1 (TermControl t2 (InstructionControl InstrPlus c)), d)
    (s, e, TermControl (TermNode _ (TmSub t1 t2)) c, d) -> Right (s, e, TermControl t1 (TermControl t2 (InstructionControl InstrSub c)), d)
    (s, e, TermControl (TermNode _ (TmMult t1 t2)) c, d) -> Right (s, e, TermControl t1 (TermControl t2 (InstructionControl InstrMult c)), d)
    (s, e, TermControl (TermNode _ (TmDiv t1 t2)) c, d) -> Right (s, e, TermControl t1 (TermControl t2 (InstructionControl InstrDiv c)), d)
    (s, e, TermControl (TermNode _ (TmIf t1 t2 t3)) c, d) -> Right (s, e, TermControl t1 (InstructionControl (InstrIf (TermControl t2 EmptyControl) (TermControl t3 EmptyControl)) c), d)
    (s, e, TermControl (TermNode _ (TmLetrec ts t1)) c, d) -> Right (EmptyStack, letrecEnvKnot e (zip (map fst ts) (map letrecKnot (map snd ts))), TermControl t1 EmptyControl, NonEmptyDump s e c d)
    (ValueStack (TermNode fi1 (TmBool b)) s, e, InstructionControl (InstrAnd c2) c, d) ->
      if b
        then Right (s, e, c2 `appendControl` c, d)
        else Right (ValueStack (TermNode fi1 (TmBool False)) s, e, c, d)
    (ValueStack (TermNode fi1 (TmBool b)) s, e, InstructionControl (InstrOr c2) c, d) ->
      if b
        then Right (ValueStack (TermNode fi1 (TmBool True)) s, e, c, d)
        else Right (s, e, c2 `appendControl` c, d)
    (ValueStack (TermNode _ (TmBool b)) s, e, InstructionControl (InstrIf c1 c2) c, d) -> Right (s, e, (if b then c1 else c2) `appendControl` c, d)
    (s, e, TermControl (TermNode _ (TmApp (TermNode _ TmFix) (TermNode _ (TmAbs f (TermNode _ (TmAbs x t1)))))) c, d) -> Right (ClosureStack x (TermControl t1 EmptyControl) ((f, TermNode noPos TmEmptyKnot) : e) s, e, c, d)
    (s, e, TermControl (TermNode _ TmFix) c, d) -> Right (s, e, TermControl yCombSubst c, d)
    (s, e, TermControl v@(TermNode _ (TmVar x)) c, d) ->
      case lookup x e of
        Just (TermNode _ (TmClosure x1 t1 e')) -> Right (ClosureStack x1 t1 e' s, e, c, d)
        Just (TermNode _ (TmTyingTheKnot x1 t1 e')) -> Right (ClosureStack x1 t1 e' s, e, c, d)
        Just r -> Right (ValueStack r s, e, c, d)
        Nothing -> Right (ValueStack v s, e, c, d)
    (s, e, TermControl (TermNode _ (TmAbs x t1)) c, d) -> Right (ClosureStack x (TermControl t1 EmptyControl) e s, e, c, d)
    (s, e, TermControl (TermNode _ (TmApp t1 t2)) c, d) -> Right (s, e, TermControl t2 (TermControl t1 (InstructionControl InstrApp c)), d)
    (ClosureStack x c' ((f, TermNode fi1 TmEmptyKnot) : e') (ValueStack t2 s), e, InstructionControl InstrApp c, d) -> Right (EmptyStack, (x, t2) : (f, TermNode fi1 (TmTyingTheKnot x c' ((f, TermNode fi1 TmEmptyKnot) : e'))) : e', c', NonEmptyDump s e c d)
    (ClosureStack x c' e' (ValueStack t2 s), e, InstructionControl InstrApp c, d) -> Right (EmptyStack, (x, t2) : e', c', NonEmptyDump s e c d)
    (ClosureStack x1 c' e' (ClosureStack x2 t2 e'' s), e, InstructionControl InstrApp c, d) -> Right (EmptyStack, (x1, TermNode noPos (TmClosure x2 t2 e'')) : e', c', NonEmptyDump s e c d)
    (ValueStack f (ValueStack v s), e, InstructionControl InstrApp c, d) | hasFun f v -> case applyFun f v of Just r -> Right (ValueStack r s, e, c, d); Nothing -> Left ("No instruction of arity 1 applies: " ++ showSECD m)
    (ValueStack v2 (ValueStack v1 s), e, InstructionControl i c, d) | hasFun2 i v1 v2 -> case applyFun2 i v1 v2 of Just r -> Right (ValueStack r s, e, c, d); Nothing -> Left ("No instruction of arity 2 applies: " ++ showSECD m)
    (ValueStack (TermNode _ TmFst) (ValueStack (TermNode _ (TmPair (TermNode _ (TmClosure x1 t1 e1)) _)) s), e, InstructionControl InstrApp c, d) -> Right (ClosureStack x1 t1 e1 s, e, c, d)
    (ValueStack (TermNode _ TmSnd) (ValueStack (TermNode _ (TmPair _ (TermNode _ (TmClosure x2 t2 e2)))) s), e, InstructionControl InstrApp c, d) -> Right (ClosureStack x2 t2 e2 s, e, c, d)
    (ValueStack (TermNode _ TmFst) (ValueStack (TermNode _ (TmPair t1 _)) s), e, InstructionControl InstrApp c, d) -> Right (ValueStack t1 s, e, c, d)
    (ValueStack (TermNode _ TmSnd) (ValueStack (TermNode _ (TmPair _ t2)) s), e, InstructionControl InstrApp c, d) -> Right (ValueStack t2 s, e, c, d)
    (ValueStack v2 (ValueStack v1 s), e, InstructionControl InstrPair c, d) -> Right (ValueStack (TermNode noPos (TmPair v1 v2)) s, e, c, d)
    (ClosureStack x2 t2 e2 (ValueStack v1 s), e, InstructionControl InstrPair c, d) -> Right (ValueStack (TermNode noPos (TmPair v1 (TermNode noPos (TmClosure x2 t2 e2)))) s, e, c, d)
    (ValueStack v2 (ClosureStack x1 t1 e1 s), e, InstructionControl InstrPair c, d) -> Right (ValueStack (TermNode noPos (TmPair (TermNode noPos (TmClosure x1 t1 e1)) v2)) s, e, c, d)
    (ClosureStack x2 t2 e2 (ClosureStack x1 t1 e1 s), e, InstructionControl InstrPair c, d) -> Right (ValueStack (TermNode noPos (TmPair (TermNode noPos (TmClosure x1 t1 e1)) (TermNode noPos (TmClosure x2 t2 e2)))) s, e, c, d)
    (ValueStack v EmptyStack, _, EmptyControl, NonEmptyDump s e c d) -> Right (ValueStack v s, e, c, d)
    (ClosureStack x t1 e' EmptyStack, _, EmptyControl, NonEmptyDump s e c d) -> Right (ClosureStack x t1 e' s, e, c, d)
    _ -> Left ("No rule applies: " ++ showSECD m)

letrecEnvKnot :: Environment -> [(Name, Environment -> TermNode)] -> Environment
letrecEnvKnot e ts = zip (map fst ts) (map (\x -> x (letrecEnvKnot e ts)) (map snd ts)) ++ e

letrecKnot :: TermNode -> (Environment -> TermNode)
letrecKnot (TermNode fi1 (TmAbs x t1)) e = TermNode fi1 (TmTyingTheKnot x (TermControl t1 EmptyControl) e)
letrecKnot t _ = t

letrecKnotControl :: (Control, Maybe Name) -> (Environment -> TermNode)
letrecKnotControl (c, Just x) e = TermNode noPos (TmTyingTheKnot x c e)
letrecKnotControl (c, _) _      = TermNode noPos (TmControl c)

yCombSubst :: TermNode
yCombSubst = TermNode noPos (TmAbs "f" (TermNode noPos (TmApp (TermNode noPos (TmAbs "x" (TermNode noPos (TmApp (TermNode noPos (TmVar "f")) (TermNode noPos (TmAbs "y" (TermNode noPos (TmApp (TermNode noPos (TmApp (TermNode noPos (TmVar "x")) (TermNode noPos (TmVar "x")))) (TermNode noPos (TmVar "y")))))))))) (TermNode noPos (TmAbs "x" (TermNode noPos (TmApp (TermNode noPos (TmVar "f")) (TermNode noPos (TmAbs "y" (TermNode noPos (TmApp (TermNode noPos (TmApp (TermNode noPos (TmVar "x")) (TermNode noPos (TmVar "x")))) (TermNode noPos (TmVar "y")))))))))))))

hasFun2 :: Instruction -> TermNode -> TermNode -> Bool
hasFun2 InstrEq (TermNode _ TmUnit) (TermNode _ TmUnit)         = True
hasFun2 InstrNE (TermNode _ TmUnit) (TermNode _ TmUnit)         = True
hasFun2 InstrLT (TermNode _ TmUnit) (TermNode _ TmUnit)         = True
hasFun2 InstrGT (TermNode _ TmUnit) (TermNode _ TmUnit)         = True
hasFun2 InstrLE (TermNode _ TmUnit) (TermNode _ TmUnit)         = True
hasFun2 InstrGE (TermNode _ TmUnit) (TermNode _ TmUnit)         = True
hasFun2 InstrEq (TermNode _ (TmInt _)) (TermNode _ (TmInt _))   = True
hasFun2 InstrNE (TermNode _ (TmInt _)) (TermNode _ (TmInt _))   = True
hasFun2 InstrLT (TermNode _ (TmInt _)) (TermNode _ (TmInt _))   = True
hasFun2 InstrGT (TermNode _ (TmInt _)) (TermNode _ (TmInt _))   = True
hasFun2 InstrLE (TermNode _ (TmInt _)) (TermNode _ (TmInt _))   = True
hasFun2 InstrGE (TermNode _ (TmInt _)) (TermNode _ (TmInt _))   = True
hasFun2 InstrEq (TermNode _ (TmBool _)) (TermNode _ (TmBool _)) = True
hasFun2 InstrNE (TermNode _ (TmBool _)) (TermNode _ (TmBool _)) = True
hasFun2 InstrLT (TermNode _ (TmBool _)) (TermNode _ (TmBool _)) = True
hasFun2 InstrGT (TermNode _ (TmBool _)) (TermNode _ (TmBool _)) = True
hasFun2 InstrLE (TermNode _ (TmBool _)) (TermNode _ (TmBool _)) = True
hasFun2 InstrGE (TermNode _ (TmBool _)) (TermNode _ (TmBool _)) = True
hasFun2 InstrPlus (TermNode _ (TmInt _)) (TermNode _ (TmInt _)) = True
hasFun2 InstrSub (TermNode _ (TmInt _)) (TermNode _ (TmInt _))  = True
hasFun2 InstrMult (TermNode _ (TmInt _)) (TermNode _ (TmInt _)) = True
hasFun2 InstrDiv (TermNode _ (TmInt _)) (TermNode _ (TmInt n2)) = n2 /= 0
hasFun2 _ _ _                                                   = False

applyFun2 :: Instruction -> TermNode -> TermNode -> Maybe TermNode
applyFun2 InstrEq (TermNode fi1 TmUnit) (TermNode _ TmUnit) = Just (TermNode fi1 (TmBool (() == ())))
applyFun2 InstrNE (TermNode fi1 TmUnit) (TermNode _ TmUnit) = Just (TermNode fi1 (TmBool (() /= ())))
applyFun2 InstrLT (TermNode fi1 TmUnit) (TermNode _ TmUnit) = Just (TermNode fi1 (TmBool (() < ())))
applyFun2 InstrGT (TermNode fi1 TmUnit) (TermNode _ TmUnit) = Just (TermNode fi1 (TmBool (() > ())))
applyFun2 InstrLE (TermNode fi1 TmUnit) (TermNode _ TmUnit) = Just (TermNode fi1 (TmBool (() <= ())))
applyFun2 InstrGE (TermNode fi1 TmUnit) (TermNode _ TmUnit) = Just (TermNode fi1 (TmBool (() >= ())))
applyFun2 InstrEq (TermNode fi1 (TmInt n1)) (TermNode _ (TmInt n2)) = Just (TermNode fi1 (TmBool (n1 == n2)))
applyFun2 InstrNE (TermNode fi1 (TmInt n1)) (TermNode _ (TmInt n2)) = Just (TermNode fi1 (TmBool (n1 /= n2)))
applyFun2 InstrLT (TermNode fi1 (TmInt n1)) (TermNode _ (TmInt n2)) = Just (TermNode fi1 (TmBool (n1 < n2)))
applyFun2 InstrGT (TermNode fi1 (TmInt n1)) (TermNode _ (TmInt n2)) = Just (TermNode fi1 (TmBool (n1 > n2)))
applyFun2 InstrLE (TermNode fi1 (TmInt n1)) (TermNode _ (TmInt n2)) = Just (TermNode fi1 (TmBool (n1 <= n2)))
applyFun2 InstrGE (TermNode fi1 (TmInt n1)) (TermNode _ (TmInt n2)) = Just (TermNode fi1 (TmBool (n1 >= n2)))
applyFun2 InstrEq (TermNode fi1 (TmBool b1)) (TermNode _ (TmBool b2)) = Just (TermNode fi1 (TmBool (b1 == b2)))
applyFun2 InstrNE (TermNode fi1 (TmBool b1)) (TermNode _ (TmBool b2)) = Just (TermNode fi1 (TmBool (b1 /= b2)))
applyFun2 InstrLT (TermNode fi1 (TmBool b1)) (TermNode _ (TmBool b2)) = Just (TermNode fi1 (TmBool (b1 < b2)))
applyFun2 InstrGT (TermNode fi1 (TmBool b1)) (TermNode _ (TmBool b2)) = Just (TermNode fi1 (TmBool (b1 > b2)))
applyFun2 InstrLE (TermNode fi1 (TmBool b1)) (TermNode _ (TmBool b2)) = Just (TermNode fi1 (TmBool (b1 <= b2)))
applyFun2 InstrGE (TermNode fi1 (TmBool b1)) (TermNode _ (TmBool b2)) = Just (TermNode fi1 (TmBool (b1 >= b2)))
applyFun2 InstrPlus (TermNode fi1 (TmInt n1)) (TermNode _ (TmInt n2)) = Just (TermNode fi1 (TmInt (n1 + n2)))
applyFun2 InstrSub (TermNode fi1 (TmInt n1)) (TermNode _ (TmInt n2)) = Just (TermNode fi1 (TmInt (n1 - n2)))
applyFun2 InstrMult (TermNode fi1 (TmInt n1)) (TermNode _ (TmInt n2)) = Just (TermNode fi1 (TmInt (n1 * n2)))
applyFun2 InstrDiv (TermNode fi1 (TmInt n1)) (TermNode _ (TmInt n2)) = Just (TermNode fi1 (TmInt (n1 `div` n2)))
applyFun2 _ _ _ = Nothing

hasFun :: TermNode -> TermNode -> Bool
hasFun (TermNode _ TmNot) (TermNode _ (TmBool _)) = True
hasFun _ _                                        = False

applyFun :: TermNode -> TermNode -> Maybe TermNode
applyFun (TermNode _ TmNot) (TermNode fi1 (TmBool b)) = Just (TermNode fi1 (TmBool (not b)))
applyFun _ _ = Nothing

type Counter = Int

isDone :: SECD -> Bool
isDone (ValueStack _ EmptyStack, [], EmptyControl, EmptyDump)       = True
isDone (ClosureStack _ _ _ EmptyStack, [], EmptyControl, EmptyDump) = True
isDone _                                                            = False

eval' :: SECD -> (Counter, Either String SECD)
eval' m = eval 0 (Right m)

eval :: Counter -> Either String SECD -> (Counter, Either String SECD)
eval n m@(Left _) = (n, m)
eval n m@(Right m')
  | isDone m' = (n, m)
  | otherwise = eval (n + 1) (eval1 m')

evalN' :: Counter -> SECD -> (Counter, Either String SECD)
evalN' n m = evalN n (Right m)

evalN :: Counter -> Either String SECD -> (Counter, Either String SECD)
evalN n m@(Left _) = (n, m)
evalN n m@(Right m')
  | n <= 0 || isDone m' = (n, m)
  | otherwise = evalN (n - 1) (eval1 m')
