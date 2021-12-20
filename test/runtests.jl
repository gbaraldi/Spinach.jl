using Spinach
using Test

@testset "Spinach.jl" begin
    ctx = LLVM.Context()
    sqrt_fun = "(define (square x) (* x x))
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
    mod = generate_IR(sqrt_fun, ctx)
    @test run_module(mod,"0") == sqrt(9)

    abs_fun = "(define (abs x) (if (< x 0) (* -1 x) x))
    (abs -2)"
    mod = generate_IR(abs_fun, ctx)
    @test run_module(mod,"0") == abs(-2.)
    fact_fun = "(define (factorial x tot) 
    (if (== x 1) tot (factorial (- x 1) (* x tot))))
    (factorial 5 1)"
    mod = generate_IR(fact_fun, ctx)
    @test run_module(mod,"0") == factorial(5)
# Write your tests here.
end
