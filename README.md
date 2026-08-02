# SECDCI
**SECD** **C**ompiler and **I**nterpreter.

## Introducing

An implementation of a SECD Compiler and interpreter as per **Lawrence Paulson's Foundations of Functional Programming \[2022\]**.

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

SECDI is an untyped language.

Lambda-abstraction, let, letrec, where and whererec defintiions can use pattern-matching for pairs. For instance: \(x, y).x. Or, for instance, let f (x, y, z) = x + y - z in f (1, 2, 3). Or, finally, for instance: g ((1, 2), (2, 3), (0, 1, 2)) where g ((x, y), (a, b), (c, d, e)) = x + y + a + b + c + d + e. It should be noted that (1, 2, 3) is equivalent to (1, (2, 3)), and \(x, y, z, a).x + y + z + a to \(x, (y, (z, a))).x + y + z + a.

Every let, letrec, where, whererec definition can include a function as follows: let f x y = x + y in f 2 3, which evaluates to 5.

Free variables are not allowed in the input.

The name of variables must begin with a lowercase letter, be followed by any numer of lowercase letters, uppercase letters, underscores and digits, and end in any number of primes.

Binary operators are required to be one of the following: +, \-, \*, /, &&, ||, <, >, <=, >=, ==, /=.

The binary operators || and && use short-circuit evaluation.

Binary operators require both operands to be of the same "type". Although this language is untyped, the same "type" here means: both operands evaluate to integers, both operands evaluate to booleans or both operands evaluate to units. This is only checked during evaluation, so "type" errors are not always caught.

Functions are treated as values.

Some (10) examples can be found within programs/default_tests.txt.

## REPL's Commands

Most of the commands are simple and related in purpose. The table is dense because there are multiple configurations for the same thing. It was not well thought out, but serves its purpose. I hope this was not too much of a hurdle.

| Command(s) | Usage | Description |
|------------|-------|------------|
| *(All commands)* | — | Command names (the first token of the command) are not case sensitive. |
| :var, :v, :assign, :a | :v \<var_name\> | Assign a written term to <var_name>. |
| :help, :h, :? | :h | Display information regarding the commands. |
| :show, :sh, :s | :s \<var_name\> | Show the term assigned to <var_name>. |
| :v, :var, :a, :assign | :v <var_name1> | Evaluate from the current environment (given <var_name2>) and store into <var_name1>. |
| :v, :var, :a, :assign | :v <var_name1> | Evaluate n-steps from the current environment and store into <var_name1>. |
| :eval, :ev, :e | :e \<var_name\> | Fully evaluate the machine assigned to <var_name>. |
| :evaln, :evn, :en | :en <number_of_steps> <var_name> | Evaluate (<number_of_steps>) n-steps the machine assigned to <var_name>. |
| :load, :l | :l \<file_path\> | Load the terms from the file at <file_path>, assigned as `<var_name> := <term>` which are then loaded into the environment as SECD machines. |
| :loadcompile, :loadc, :lcompile, :lc | :lc \<file_path\> | Load the terms from the file at <file_path>, assigned as in `<var_name> := <term>`, which are then compiled and loaded into the environment as SECD machines. |
| :ee, :eenv, :evalenv | :evalenv | Attempt to evaluate all variables in the environment. |
| :v?, :vars | :v? | Show the first page (10 environment variables) if a number is not specified. |
| :v?, :vars | :v? \<number\> | Show the <number>'th page (containing 10 environment variables' names). |
| :m, :mv, :move | :mv \<var_name1\> \<var_name2\> | Store the contents of <var_name2> into <var_name1>. |
| :q, :quit | :q | Close the REPL. |
| :c, :ce, :cenv, :clear, :clearenv | :c | Clear the environment (no variables accessible until new ones are added). |
| :av?, :allvars | :av? | Show all variables in the environment. |
| :se, :senv, :showe, :showenv | :se | Show the environment. |
| :se, :senv, :showe, :showenv | :se \<page_number\> | Show a specific environment page. |
| :ee, :eenv, :evalenv | :ee \<page_number\> | Evaluate a specific environment page. |
| \<program\> | \<program\> | Shows and evaluates the given program/term/machine, and then compiles it and shows and evaluates. |
| *(Environment pages)* | — | Page numbers start at 1. |

## Reporting issues

Do not forget to report any bugs. Contact me, otherwise you can create a new issue on this repository. Thanks!
