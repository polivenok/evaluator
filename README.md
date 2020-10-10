# evaluator

Create a set of functions with corresponding unit test 
for evaluating and optimizing simple arithmetic expressions.
Allowed operators: `+`, `-`, `*`, `/` (bonus points for functions like `power`, `abs`)
1. Write a simple evaluator function and test it
2. Add variables map to evaluator and test it
3. Write an `optimize` function that simplifies the expression using arithmetic rules
   - `x * 0 = 0`
   - `x * 1 = x`
   - `1 * x = x`
   - and others for addition, substraction and division
4. Write a function `->javascript` that converts expressions into javascript function

Example:
```clj
(evaluate {} '(* 2 (+ 1 1))) => 4

(evaluate {:x 10} '(* x x)) => 100

(optimize '(+ 10 (* x 0))) => 10

(optimize '(+ x (- y 0))) => '(+ x y)

(->javascript "example" '(+ 1 (* x x))) =>
  "function example(x) { return (1 + (x * x)); }"
```


