include("codegen.jl")

sqrt = "(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))
(define (abs x) (if (< x 0) (* -1 x) x))
(define (improve guess x) (average guess (/ x guess)))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x) (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
(define (sqrt x) (sqrt-iter 1.00 x))
(sqrt 9)
"
abs = "(define (abs x) (if (< x 0) (* -1 x) x))"
prod = "(define (produtorio x tot) 
    (if (== x 1) tot (produtorio (- x 1) (* x tot))))"
str3 = "(define var 3)
(define (foo x) (+ x var))
(foo 2)
(define (fatorial x tot) 
(if (== x 1) tot (fatorial (- x 1) (* x tot))))
(fatorial 5 1)
(if (> 3 var) 1 3)
"


ctx = LLVM.Context()
mod = generate_IR(str2, ctx)
run(mod,"0")

