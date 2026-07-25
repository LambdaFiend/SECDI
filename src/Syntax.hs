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
  | TmFix
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
  | TmPair TermNode TermNode
  | TmFst
  | TmSnd
  | TmTyingTheKnot Name TermNode Environment
  | TmEmptyKnot
  | TmClosure Name TermNode Environment
  | TmLetrec [(Name, TermNode)] TermNode
  | TmError String
  deriving (Eq, Show)

type SECD = (Stack, Environment, Control, Dump)

data Instruction
  = InstrApp
  | InstrAnd
  | InstrOr
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
  | InstrIf TermNode TermNode
  deriving (Show, Eq)

data Stack
  = ClosureStack Name TermNode Environment Stack
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
