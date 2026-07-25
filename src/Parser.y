{
module Parser where

import Lexer
import Syntax
}

%monad { Either String } { (>>=) } { return }
%name parser
%tokentype { Token }
%error { parseError }

%token

let      { Token pos LET }
letrec   { Token pos LETREC }
where    { Token pos WHERE }
whererec { Token pos WHEREREC }
in       { Token pos IN }
true     { Token pos TRUE }
false    { Token pos FALSE }
"&&"     { Token pos AND }
"||"     { Token pos OR }
and      { Token pos ANDWORD }
not      { Token pos NOT }
if       { Token pos IF }
then     { Token pos THEN }
else     { Token pos ELSE }
fst      { Token pos FST }
snd      { Token pos SND }
fix      { Token pos FIX }
"="      { Token pos ASSIGN }
"<"      { Token pos LESST }
">"      { Token pos GREATT }
"<="     { Token pos LESSE }
">="     { Token pos GREATE }
"=="     { Token pos EQUAL }
"/="     { Token pos NEQUAL }
"+"      { Token pos PLUS }
"-"      { Token pos MINUS }
"*"      { Token pos MULT }
"/"      { Token pos DIV }
","      { Token pos COMMA }
"_"      { Token pos UNDERSCORE }
"\\"     { Token pos LAMBDA }
"."      { Token pos DOT }
"("      { Token pos LPAREN }
")"      { Token pos RPAREN }
id       { Token pos (ID s) }
int      { Token pos (LITINT s) }

%%

Term
  : If                    { $1 }
  | Let                   { $1 }
  | letrec Letrec in Term { TermNode (tokenPos $1) (TmLetrec $2 $4) }
  | LogicOp               { $1 }
  | "\\" Abs              { $2 }

If : if Term then Term else Term { TermNode (tokenPos $1) (TmIf $2 $4 $6) }

Let
  : let NamePattern1 "=" Term in Term              { TermNode (tokenPos $1) (TmApp ($2 $6) $4) }
  | let NamePattern1 LetMany "=" Term in Term      { TermNode (tokenPos $1) (TmApp ($2 $7) ($3 $5)) }
  | LogicOp where NamePattern1 "=" Term            { TermNode (tokenPos $2) (TmApp ($3 $1) $5) }
  | LogicOp where NamePattern1 LetMany "=" Term    { TermNode (tokenPos $2) (TmApp ($3 $1) ($4 $6)) }
  | LogicOp whererec Name "=" Term         { TermNode (tokenPos $2) (TmApp (TermNode (tokenPos $4) (TmAbs (snd $3) $1)) (TermNode (tokenPos $4) (TmApp (TermNode (tokenPos $4) TmFix) (TermNode (tokenPos $4) (TmAbs (snd $3) $5))))) }
  | LogicOp whererec Name LetMany "=" Term { TermNode (tokenPos $2) (TmApp (TermNode (tokenPos $5) (TmAbs (snd $3) $1)) (TermNode (tokenPos $5) (TmApp (TermNode (tokenPos $5) TmFix) (TermNode (tokenPos $5) (TmAbs (snd $3) ($4 $6)))))) }

LetMany :: { TermNode -> TermNode }
  : Name LetMany        { \x -> TermNode (fst $1) (TmAbs (snd $1) ($2 x)) }
  | Name                { \x -> TermNode (fst $1) (TmAbs (snd $1) x) }
  | AbsPattern1 LetMany { \x -> $1 ($2 x) }
  | AbsPattern1         { $1 }

Letrec
  : Name "=" Term and Letrec         { (snd $1, $3) : $5 }
  | Name LetMany "=" Term and Letrec { (snd $1, $2 $4) : $6 }
  | Name "=" Term                    { (snd $1, $3) : [] }
  | Name LetMany "=" Term            { (snd $1, $2 $4) : [] }

Abs
  : Name "." Term        { TermNode (fst $1) (TmAbs (snd $1) $3) }
  | Name Abs             { TermNode (fst $1) (TmAbs (snd $1) $2) }
  | "_" "." Term         { TermNode (tokenPos $1) (TmAbs "_" $3) }
  | "_" Abs              { TermNode (tokenPos $1) (TmAbs "_" $2) }
  | AbsPattern1 "." Term { $1 $3 }

AbsPattern1 :: { TermNode -> TermNode }
  : "(" NamePattern1 "," NamePattern2 ")" { \x -> TermNode (tokenPos $3) (TmAbs "_" (TermNode (tokenPos $3) (TmApp (TermNode (tokenPos $3) (TmApp ($2 ($4 x)) (TermNode (tokenPos $3) (TmApp (TermNode (tokenPos $3) TmFst) (TermNode (tokenPos $3) (TmVar "_")))))) (TermNode (tokenPos $3) (TmApp (TermNode (tokenPos $3) TmSnd) (TermNode (tokenPos $3) (TmVar "_"))))))) }

NamePattern1 :: { TermNode -> TermNode }
  : Name        { \x -> TermNode (fst $1) (TmAbs (snd $1) x) }
  | AbsPattern1 { $1 }

NamePattern2 :: { TermNode -> TermNode }
  : Name        { \x -> TermNode (fst $1) (TmAbs (snd $1) x) }
  | AbsPattern1 { $1 }
  | AbsPattern2 { $1 }

AbsPattern2 :: { TermNode -> TermNode }
  : NamePattern1 "," NamePattern2 { \x -> TermNode (tokenPos $2) (TmAbs "_" (TermNode (tokenPos $2) (TmApp (TermNode (tokenPos $2) (TmApp ($1 ($3 x)) (TermNode (tokenPos $2) (TmApp (TermNode (tokenPos $2) TmFst) (TermNode (tokenPos $2) (TmVar "_")))))) (TermNode (tokenPos $2) (TmApp (TermNode (tokenPos $2) TmSnd) (TermNode (tokenPos $2) (TmVar "_"))))))) }

LogicOp
  : LogicOp "&&" RelOp { TermNode (tokenPos $2) (TmAnd $1 $3) }
  | LogicOp "||" RelOp  { TermNode (tokenPos $2) (TmOr $1 $3) }
  | RelOp             { $1 }

RelOp
  : PlusOp "==" PlusOp { TermNode (tokenPos $2) (TmEq $1 $3) }
  | PlusOp "/=" PlusOp { TermNode (tokenPos $2) (TmNE $1 $3) }
  | PlusOp "<" PlusOp  { TermNode (tokenPos $2) (TmLT $1 $3) }
  | PlusOp ">" PlusOp  { TermNode (tokenPos $2) (TmGT $1 $3) }
  | PlusOp "<=" PlusOp { TermNode (tokenPos $2) (TmLE $1 $3) }
  | PlusOp ">=" PlusOp { TermNode (tokenPos $2) (TmGE $1 $3) }
  | PlusOp             { $1 }

PlusOp
  : PlusOp "+" MultOp { TermNode (tokenPos $2) (TmPlus $1 $3) }
  | PlusOp "-" MultOp { TermNode (tokenPos $2) (TmSub $1 $3) }
  | MultOp            { $1 }

MultOp
  : MultOp "*" UnaryOp { TermNode (tokenPos $2) (TmMult $1 $3) }
  | MultOp "/" UnaryOp { TermNode (tokenPos $2) (TmDiv $1 $3) }
  | UnaryOp            { $1 }

UnaryOp
  : "-" App { TermNode (tokenPos $1) (TmSub (TermNode (tokenPos $1) (TmInt 0)) $2) }
  | App     { $1 }

App
  : App Atom  { TermNode (getFI $1) (TmApp $1 $2) }
  | Atom      { $1 }

Atom
  : Var          { $1 }
  | Value        { $1 }
  | Pair         { $1 }
  | "(" Term ")" { $2 }

Var : Name { TermNode (fst $1) (TmVar (snd $1)) }

Name : id { (tokenPos $1, (\(ID s) -> s) (tokenDat $1)) }

Value
  : int     { TermNode (tokenPos $1) (TmInt ((\(LITINT n) -> n) (tokenDat $1))) }
  | true    { TermNode (tokenPos $1) (TmBool True) }
  | false   { TermNode (tokenPos $1) (TmBool False) }
  | fst     { TermNode (tokenPos $1) TmFst }
  | snd     { TermNode (tokenPos $1) TmSnd }
  | not     { TermNode (tokenPos $1) TmNot }
  | fix     { TermNode (tokenPos $1) TmFix }
  | "(" ")" { TermNode (tokenPos $1) TmUnit }

Pair : "(" Term "," PairMany ")" { TermNode (tokenPos $1) (TmPair $2 $4) }

PairMany
  : Term "," PairMany { TermNode (getFI $1) (TmPair $1 $3) }
  | Term              { $1 }

{
parseError :: [Token] -> Either String a
parseError []                    = Left ("Parsing error near the end of the file")
parseError ((Token fi _):tokens) = Left ("Parsing error at:" ++ showFileInfoHappy fi)
parseError (x:xs)                = Left "Parsing error"

showFileInfoHappy :: AlexPosn -> String
showFileInfoHappy (AlexPn p l c) =
  "\n" ++ "Absolute Offset: " ++ show p ++ "\n"
  ++ "Line: " ++ show l ++ "\n"
  ++ "Column: " ++ show c
}

