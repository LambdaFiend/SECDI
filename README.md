# SECDI
**SECD** **I**nterpreter.

## Introducing

An implementation of a SECD interpreter as per **Lawrence Paulson's Foundations of Functional Programming \[2022\]**.

## Running the program

Haskell and Cabal should both be installed.

```cabal build```

```cabal run```

Type ```:?```, ```:h``` or ```:help``` for help within the program.

## Syntax and Semantics

| Syntax | Semantics |
| :----: | :-------- |
| i | An integer literal, which may be positive (e.g. 3), negative (e.g. 2) or zero (0) |
| b | A boolean literal, which may be true (true) or false (false) |
| () | An unit literal |
| x | A term variable |
| \x.t | A lambda-abstraction where x is bound within t and an argument is expected to be passed |
| t1 t2 | An application, which complements lambda-abstractions as the means to pass arguments |
| (t1, t2) | A pair, where t1 and t2 may be any term |
| fst t | The fst projection function, which extracts the first (or left-side) element of a pair |
| snd t | The snd projection function, which extracts the second (or right-side) element of a pair |
| t1 op t2 | An infix binary operator |
| not t | The boolean negation operator, turning true into false and false into true |
| if t1 then t2 else t3 | The if-then-else expression, which requires t1 to evaluate to a boolean and only evaluates t2 if t1 evaluates to true and only evaluates t3 if t1 evaluates to false |
| let x = t1 in t2 | A let-binding, where x is bound to t1 within t2 |
| t2 where x = t1 | A where-binding (post-hoc), where x is bound to t1 within t2 |
| letrec f x = t1 in t2 | A let-binding for recursive functions, such that the function f appears bound not just within t2 but also within t1, and x is an argument of f (there can be more arguments in the same fashion of x) |
| t2 whererec f x = t1 in t2 | A where-binding for recursive functions, such that the function f appears bound not just within t2 but also within t1, and x is an argument of f (there can be more arguments in the same fashion of x) |
| letrec f x = t1 and g y = t2 and ... in tn | A let-binding for mutually recursive functions, such that f appears bound in every t1, t2, ..., tn |
| Y | The Y combinator, useful for recursive definitions |

Every let, letrec, where, whererec definition can include a function as follows: let f x y = x + y in f 2 3, which evaluates to 5.

The definition of letrec and whererec must be that of a function, so at least one function argument must be given: letrec f x = x + 1 in f 4, which evaluates to 5.

Free variables are not allowed as input.

The name of variables must begin with a lowercase letter, be followed by any numer of lowercase letters, uppercase letters, underscores and digits, and end in any number of primes.

Binary operators are required to be one of the following: +, \-, \*, /, &&, ||, <, >, <=, >=, ==, /=.

The binary operators || and && use short-circuit evaluation.

Binary operators require both operands to be of the same "type". Although this language is untyped, the same "type" here means: both operands evaluate to integers, both operands evaluate to booleans or both operands evaluate to units.

## Reporting issues

Do not forget to report any bugs. Contact me, otherwise you can create a new issue on this repository. Thanks!
