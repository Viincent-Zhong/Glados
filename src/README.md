# Glados

## Types
- <span style="color:orange">Integer</span> Int
- <span style="color:orange">Symbol</span> String
- <span style="color:orange">Boolean</span> #t <sub>or </sub> #f
- <span style="color:orange">Expression</span> (...)   &nbsp; <sub>Start by parenthesis and end by parenthesis </sub>

#### Example of valid s-expressions
```lisp
foo
42
(1 2 3)
((foo bar baz)
    (1 2 3) ()
    ((((1 (2) 3))))
   )
```


## <span style="color:red">Lambda</span> syntaxes
Syntax : (lambda (arg1 arg2 ... argN) body)
```lisp
((lambda (a b) (+ a b)) 1 2)
```

## <span style="color:red">Define</span> syntaxes
#### Assigning a value
Syntax : (define func_name integer)
```lisp
(define foo 42)
```
#### Assigning a variable
Syntax : (define func_name variable)
```lisp
(define foo x)
; should be an error if x is undefined
```
#### Assigning a function
Syntax : (define (func_name [arg1 arg2 ... argN]) (body))
```lisp
(define (foo a b) (if (eq? a b) a b))
```

#### Assigining a lambda
Syntax : (define func_name lambda)
```lisp
(define add
    (lambda (a b)
        (+ a b)))
(add 3 4)
```

## <span style="color:red">If</span> syntaxes
Syntax : (if condition then else)
```lisp
(define foo 42)
(if (< foo 10)
    (* foo 3) ; then return foo * 3
    (div foo 2)) ; else return foo / 2
```

## <span style="color:red">Builtins</span> functions
- Eq?
```lisp
(eq? 1 2) ; => #f
(eq? 1 1) ; => #t
```
- <
```lisp
(< 1 10) ; => #t
(< 10 1) ; => #f
(< 1 1) ; => #f
```
- \+
```lisp
(+ 1 2) ; => 3
```
- \-
```lisp
(- 2 1) ; => 1
```
- \*
```lisp
(* 2 2) ; => 4
```
- div
```lisp
(div 3 1) ; => 3
```
- mod
```lisp
(mod 2 1) ; => 0
```

