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

;--------------------------------
;            HW11
;--------------------------------

(defn proto-get [obj key]
  (cond
    (contains? obj key) (obj key)
    (contains? obj :prototype) (proto-get (obj :prototype) key)
    :else nil))

(defn proto-call [this key & args]
  (apply (proto-get this key) this args))

(defn constructor [ctor prototype]
  (fn [& args] (apply ctor {:prototype prototype} args)))

(defn field [key]
  (fn [this] (proto-get this key)))

(defn method [key]
  (fn [this & args] (apply proto-call this key args)))

(def evaluate (method :evaluate))
(def toString (method :toString))
(def diff (method :diff))

(declare ZERO)
(def Constant (let [value (field :value)]
                (constructor (fn [this value] (assoc this :value value))
                             {:evaluate (fn [this _] (value this))
                              :toString (fn [this] (format "%.1f" (value this)))
                              :diff     (fn [_ _] ZERO)})))
(def ZERO (Constant 0))
(def ONE (Constant 1))

(def Variable (let [name (field :name)]
                (constructor (fn [this name] (assoc this :name name))
                             {:evaluate (fn [this vars] (get vars (name this)))
                              :toString (fn [this] (name this))
                              :diff     (fn [this d-name] (if (= d-name (name this))
                                                            ONE
                                                            ZERO))})))

(def Op-proto (let [operands (field :operands)
                    name (field :name)
                    calc-func (field :calc-func)
                    diff-func (field :diff-func)]
                {:evaluate (fn [this vars] (apply (calc-func this) (mapv #(evaluate % vars) (operands this))))
                 :toString (fn [this] (str "(" (name this) " "
                                           (clojure.string/join " " (mapv toString (operands this)))
                                           ")"))
                 :diff     (fn [this d-name] ((diff-func this) (operands this)  (mapv #(diff % d-name) (operands this))))}))

(defn make-operation [name calc-func diff-func]
  (constructor (fn [this & operands] (assoc this :operands (vec operands)))
               {:prototype Op-proto
                :name      name
                :calc-func calc-func
                :diff-func diff-func}))

(declare Add)
(defn diff-sum [_ d-args] (apply Add d-args))

(def Add (make-operation "+" + diff-sum))

(def Subtract (make-operation "-" -
                              (fn [args d-args] (apply Subtract d-args))))

(declare Multiply)
(defn diff-mult [args d-args] (apply Add (mapv #(apply Multiply (assoc args % (nth d-args %)))
                                               (range (count args)))))

(def Multiply (make-operation "*" * diff-mult))

(def Negate (make-operation "negate" (fn [a] (- a))
                            (fn [args d-args] (Negate (first d-args)))))

(def Divide (make-operation "/" (fn ([a] (/ (double a)))
                                  ([a & args] (/ (double a) (double (apply * args)))))
                            (fn [args d-args] (let [a  (first args)
                                                    da (first d-args)
                                                    b  (apply Multiply (apply vector (rest args)))
                                                    db (diff-mult (apply vector (rest args))
                                                                  (apply vector (rest d-args)))]
                                                (if (== 1 (count args))
                                                  (Divide (Negate da)
                                                          (Multiply a a))
                                                  (Divide (Subtract (Multiply da b)
                                                                    (Multiply a db))
                                                          (Multiply b b)))))))

(def Sum (make-operation "sum" + diff-sum))

(def Avg (make-operation "avg"
                         (fn [& args] (/ (double (apply + args)) (double (count args))))
                         (fn [args d-args] (apply Avg d-args))))

(defn parseObject [expr] (common-parse {'+      Add
                                        '-      Subtract
                                        '*      Multiply
                                        '/      Divide
                                        'sum    Sum
                                        'avg    Avg
                                        'negate Negate} Constant Variable expr))