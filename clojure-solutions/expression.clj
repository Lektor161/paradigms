;--------------------------------
;            HW10
;--------------------------------
(def constant constantly)
(defn variable [name] (fn [vars] (get vars name)))

(defn abstract-operation [f] (fn [& operands] (fn [vars] (apply f (mapv (fn [operand] (operand vars)) operands)))))

(def add (abstract-operation +))
(def subtract (abstract-operation -))
(def multiply (abstract-operation *))
(def divide (abstract-operation (fn ([a] (/ (double a)))
                                  ([a & args] (/ (double a) (double (apply * args)))))))
(def negate (abstract-operation (fn [a] (- a))))
(def med (abstract-operation (fn [& args] (nth (sort args) (quot (count args) 2)))))
(def avg (abstract-operation (fn [& args] (/ (double (apply + args)) (double (count args))))))

(defn common-parse [tokens cnst vr input]
  (letfn [(do-parse [expr] (cond
                             (list? expr) (apply (get tokens (first expr)) (mapv do-parse (rest expr)))
                             (number? expr) (cnst expr)
                             :else (vr (str expr))))]
    (do-parse (read-string input))))

(defn parseFunction [expr]
  (common-parse {'+      add
                 '-      subtract
                 '*      multiply
                 '/      divide
                 'negate negate
                 'med    med
                 'avg    avg} constant variable expr))