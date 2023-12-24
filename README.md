# Sample implementation of languages in "Types and Programming Languages"

This repository is for learning [Types and Programming Languages](https://www.amazon.com/dp/0262162091), and contains a Haskell implementation of the languages that is introduced in that book.


## Running

You can play with REPL. Type the follow command to try:

```shell
$ stack run
```

Type `:q` to quit REPL.

By default, REPL displays only the final form of the expression.
If you want to see the evaluation step by step, please run with `-s` or `--step-evaluation` option.

Also you can specify a target language of the REPL by using `-t` option.
The default is `section3`, a language introduced in Untyped Arithmetic Expression.
Please use `-h` or `--help` to see other choices.


## Examples

Default (the target language is `section3` and display only the final form):

```shell
$ stack run
type> if iszero pred succ 0 then if false then true else iszero succ pred succ 0 else true
false
type> if false then 0 else iszero succ pred false
iszero (succ (pred false))
```

Use step-by-step evaluation:

```shell
$ stack run -- -s
type> if iszero pred succ 0 then if false then true else iszero succ pred succ 0 else true
   if (iszero (pred (succ 0))) then (if false then true else (iszero (succ (pred (succ 0))))) else true
-> if (iszero 0) then (if false then true else (iszero (succ (pred (succ 0))))) else true
-> if true then (if false then true else (iszero (succ (pred (succ 0))))) else true
-> if false then true else (iszero (succ (pred (succ 0))))
-> iszero (succ (pred (succ 0)))
-> iszero (succ 0)
-> false
```

Specify a target language and use step-by-step evaluation:

```shell
$ stack run -- -s -t section3-wrong
type> if false then 0 else iszero succ pred false
   if false then 0 else (iszero (succ (pred false)))
-> iszero (succ (pred false))
-> iszero (succ wrong)
-> iszero wrong
-> wrong
```
