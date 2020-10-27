(ns evaluator.core
  (:use clojure.walk))

(defn to-map-with-symbol-keys
  "From map {a: 10} to map {a 10}"
  [map]
  (into {}
        (for [[k v] map]
          [(symbol k) v])))

(defn evaluate
  "Evaluates arithmetic expressions.
  (evaluate {:x 10} '(* x x)) => 100"
  [map exp]
  (let [b-vec (reduce into [] (to-map-with-symbol-keys map))]
    (eval `(let ~b-vec ~exp))))

(defn math-expression?
  [exp]
  (and (seq? exp)
       (contains? #{'+ '* '/ '-} (first exp))))

(defn apply-rules
  "Simplifies the expression using arithmetic rules:
   - `x * 0 = 0`
   - `x * 1 = x`
   - `x * 1 = x`
   - `1 * x = x`
   - and others for addition, substraction and division"
  [exp]
  (let [[op first second] exp]
    (case op
      * (cond
          (= first 1) second
          (= second 1) first
          (or (= first 0) (= second 0)) 0
          :else exp)
      - (cond
          (= second 0) first
          :else exp)
      ; TODO: add more rules
      exp)))

(defn eval-numeric
  "If expression doesn't contain symbol between operators - evaluate it, otherwise return the original one
    (+ 3 2) => 5
    (+ x y) => (+ x y)"
  [exp]
  (if (math-expression? exp)
    (let [[_ first second] exp]
      (if (or (symbol? first) (symbol? second)) exp
                                                (eval exp)))
    exp))

(defmacro optimize [& code]
  "macro that simplifies the expression using arithmetic rules
  (optimize '(+ 10 (* x 0))) => 10
  (optimize '(+ x (- y 0))) => '(+ x y)
  "
  `(do
     ~@(postwalk (fn [block]
                   (if (math-expression? block)
                     (-> block
                         apply-rules
                         eval-numeric)
                     block))
                 code)))

(defn to-infix-str
  "Returns string in format: 'f' 'op' 's', e.g
  '(to-infix-text + 3 2)=> \"3 + 2\""
  [op f s]
  (str f " " op " " s))

(defn to-infix
  " Transforms simple arithmetic expression in prefix notation to infix notation.
    Returns string.
  '(+ 1 (* x x))) => \"1 + x * x\" "
  [code]
  (loop [stack []
         r-code (reverse (flatten code))]
    (if (not-empty r-code)
      (let [item (first r-code)]
        (if (or (= item '+) (= item '-) (= item '*) (= item '/))
          (recur
            (cons (to-infix-str item (first stack) (second stack)) (drop 2 stack))
            (next r-code))
          (recur (cons item stack) (next r-code))))
      (first stack))))

(defn ->javascript
  "Generates javascript function based on arithmetic expression with single local param 'x'"
  [name code]
  (str "function " name "(x) { return " (to-infix code) "; }"))

