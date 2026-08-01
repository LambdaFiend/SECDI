{
module Lexer where
}

%wrapper "posn"

$white         = [\ \t\n\r\b]
$lower         = [a-z]
$upper         = [A-Z]
$digit         = [0-9]
$alpha         = [a-zA-Z]
$alphanumunder = [a-zA-Z0-9_]

tokens :-

$white+                        ;
let                            { \pos _ -> Token pos LET }
letrec                         { \pos _ -> Token pos LETREC }
where                          { \pos _ -> Token pos WHERE }
whererec                       { \pos _ -> Token pos WHEREREC }
in                             { \pos _ -> Token pos IN }
true                           { \pos _ -> Token pos TRUE }
false                          { \pos _ -> Token pos FALSE }
"&&"                           { \pos _ -> Token pos AND }
"||"                           { \pos _ -> Token pos OR }
and                            { \pos _ -> Token pos ANDWORD }
not                            { \pos _ -> Token pos NOT }
if                             { \pos _ -> Token pos IF }
then                           { \pos _ -> Token pos THEN }
else                           { \pos _ -> Token pos ELSE }
fst                            { \pos _ -> Token pos FST }
snd                            { \pos _ -> Token pos SND }
Y                              { \pos _ -> Token pos FIX }
"="                            { \pos _ -> Token pos ASSIGN }
"<"                            { \pos _ -> Token pos LESST }
">"                            { \pos _ -> Token pos GREATT }
"<="                           { \pos _ -> Token pos LESSE }
">="                           { \pos _ -> Token pos GREATE }
"=="                           { \pos _ -> Token pos EQUAL }
"/="                           { \pos _ -> Token pos NEQUAL }
"+"                            { \pos _ -> Token pos PLUS }
"-"                            { \pos _ -> Token pos MINUS }
"*"                            { \pos _ -> Token pos MULT }
"/"                            { \pos _ -> Token pos DIV }
","                            { \pos _ -> Token pos COMMA }
"_"                            { \pos _ -> Token pos UNDERSCORE }
\\|"λ"                         { \pos _ -> Token pos LAMBDA }
"."                            { \pos _ -> Token pos DOT }
"("                            { \pos _ -> Token pos LPAREN }
")"                            { \pos _ -> Token pos RPAREN }
($lower)($alphanumunder|\')*   { \pos s -> Token pos (ID s) }
$digit+                        { \pos s -> Token pos (LITINT (read s)) }
.                              { \pos s -> Token pos (ERROR ("Lexing error: " ++ s)) }

{
data Token = Token
  { tokenPos :: AlexPosn
  , tokenDat :: TokenData
  }
  deriving (Show, Eq)

data TokenData
  = ERROR String
  | LET
  | LETREC
  | WHERE
  | WHEREREC
  | IN
  | TRUE
  | FALSE
  | AND
  | OR
  | ANDWORD
  | NOT
  | IF
  | THEN
  | ELSE
  | FST
  | SND
  | FIX
  | ASSIGN
  | LESST
  | GREATT
  | LESSE
  | GREATE
  | EQUAL
  | NEQUAL
  | PLUS
  | MINUS
  | MULT
  | DIV
  | COMMA
  | UNDERSCORE
  | LAMBDA
  | DOT
  | LPAREN
  | RPAREN
  | ID String
  | LITINT Int
  deriving (Show, Eq)
}
