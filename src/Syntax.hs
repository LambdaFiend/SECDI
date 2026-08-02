module Syntax where

import           Lexer

type Index = Int

type Name = String

type Environment = [(Name, TermNode)]

type FileInfo = AlexPosn

data TermNode = TermNode
  { getFI :: FileInfo,
    getTm :: Term
  }
  deriving (Eq, Show)

data Term
  = TmVar Name
  | TmAbs Name TermNode
  | TmApp TermNode TermNode
  | TmIf TermNode TermNode TermNode
  | TmAnd TermNode TermNode
  | TmOr TermNode TermNode
  | TmEq TermNode TermNode
  | TmNE TermNode TermNode
  | TmLT TermNode TermNode
  | TmGT TermNode TermNode
  | TmLE TermNode TermNode
  | TmGE TermNode TermNode
  | TmPlus TermNode TermNode
  | TmSub TermNode TermNode
  | TmMult TermNode TermNode
  | TmDiv TermNode TermNode
  | TmNot
  | TmInt Int
  | TmBool Bool
  | TmUnit
  | TmPair TermNode TermNode
  | TmFst
  | TmSnd
  | TmFix
  | TmLetrec [(Name, TermNode)] TermNode
  | TmTyingTheKnot Name Control Environment
  | TmEmptyKnot
  | TmClosure Name Control Environment
  | TmControl Control
  | TmError String
  deriving (Eq, Show)

type SECD = (Stack, Environment, Control, Dump)

data Instruction
  = InstrApp
  | InstrAnd Control
  | InstrOr Control
  | InstrEq
  | InstrNE
  | InstrLT
  | InstrGT
  | InstrLE
  | InstrGE
  | InstrPlus
  | InstrSub
  | InstrMult
  | InstrDiv
  | InstrPair
  | InstrIf Control Control
  | InstrConstInt Int
  | InstrConstBool Bool
  | InstrConstUnit
  | InstrConstFst
  | InstrConstSnd
  | InstrConstNot
  | InstrVar Name
  | InstrClosure Name Control
  | InstrKnot Name Name Control
  | InstrLetrec [(Name, (Control, Maybe Name))] Control
  deriving (Show, Eq)

data Stack
  = ClosureStack Name Control Environment Stack
  | ValueStack TermNode Stack
  | EmptyStack
  deriving (Show, Eq)

data Control
  = InstructionControl Instruction Control
  | TermControl TermNode Control
  | EmptyControl
  deriving (Show, Eq)

data Dump
  = NonEmptyDump Stack Environment Control Dump
  | EmptyDump
  deriving (Show, Eq)

fromMaybe :: Maybe a -> a
fromMaybe (Just x) = x
fromMaybe Nothing = error "ERROR: got Nothing when applying fromMaybe in Syntax.hs"

noPos :: FileInfo
noPos = AlexPn (-1) (-1) (-1)

isVal :: TermNode -> Bool
isVal t =
  case getTm t of
    TmAbs _ _                           -> True
    TmPair t1 t2 | isVal t1 && isVal t2 -> True
    TmInt _                             -> True
    TmBool _                            -> True
    _                                   -> False

emptySECD :: SECD
emptySECD = (EmptyStack, [], EmptyControl, EmptyDump)

createSECD :: TermNode -> SECD
createSECD t = (EmptyStack, [], TermControl t EmptyControl, EmptyDump)

type NameContext = [Name]

findFreeVars :: NameContext -> TermNode -> Maybe NameContext
findFreeVars ctx t =
  case getTm t of
    TmVar x -> if elem x ctx then Nothing else Just [x]
    TmAbs x t1 -> findFreeVars (x : ctx) t1
    TmApp t1 t2 -> findFreeVars ctx t1 <> findFreeVars ctx t2
    TmIf t1 t2 t3 -> findFreeVars ctx t1 <> findFreeVars ctx t2 <> findFreeVars ctx t3
    TmFix -> Nothing
    TmAnd t1 t2 -> findFreeVars ctx t1 <> findFreeVars ctx t2
    TmOr t1 t2 -> findFreeVars ctx t1 <> findFreeVars ctx t2
    TmEq t1 t2 -> findFreeVars ctx t1 <> findFreeVars ctx t2
    TmNE t1 t2 -> findFreeVars ctx t1 <> findFreeVars ctx t2
    TmLT t1 t2 -> findFreeVars ctx t1 <> findFreeVars ctx t2
    TmGT t1 t2 -> findFreeVars ctx t1 <> findFreeVars ctx t2
    TmLE t1 t2 -> findFreeVars ctx t1 <> findFreeVars ctx t2
    TmGE t1 t2 -> findFreeVars ctx t1 <> findFreeVars ctx t2
    TmPlus t1 t2 -> findFreeVars ctx t1 <> findFreeVars ctx t2
    TmSub t1 t2 -> findFreeVars ctx t1 <> findFreeVars ctx t2
    TmMult t1 t2 -> findFreeVars ctx t1 <> findFreeVars ctx t2
    TmDiv t1 t2 -> findFreeVars ctx t1 <> findFreeVars ctx t2
    TmNot -> Nothing
    TmInt _ -> Nothing
    TmBool _ -> Nothing
    TmPair t1 t2 -> findFreeVars ctx t1 <> findFreeVars ctx t2
    TmFst -> Nothing
    TmSnd -> Nothing
    TmTyingTheKnot _ _ _ -> Nothing
    TmEmptyKnot -> Nothing
    TmClosure _ _ _ -> Nothing
    TmLetrec ts t1 -> (\x -> if x == [] then Nothing else Just x) (concat $ helperFun (map (\(x, y) -> findFreeVars ([x] ++ map fst ts ++ ctx) y) ts)) <> findFreeVars (map fst ts ++ ctx) t1
    TmUnit -> Nothing
    TmControl _ -> Nothing
    TmError _ -> Nothing
  where
    helperFun []             = []
    helperFun (Just x : xs)  = x : helperFun xs
    helperFun (Nothing : xs) = helperFun xs

appendControl :: Control -> Control -> Control
appendControl EmptyControl c2 = c2
appendControl (TermControl t c1) c2 = TermControl t (appendControl c1 c2)
appendControl (InstructionControl i c1) c2 = InstructionControl i (appendControl c1 c2)

isConst :: TermNode -> Bool
isConst (TermNode _ (TmInt _))  = True
isConst (TermNode _ (TmBool _)) = True
isConst (TermNode _ TmUnit)     = True
isConst _                       = False
