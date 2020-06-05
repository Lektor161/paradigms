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

(defn common-parse [tokens cnst vr]
  (fn [input] (letfn [(do-parse [expr] (cond
                                         (list? expr) (apply (get tokens (first expr)) (mapv do-parse (rest expr)))
                                         (number? expr) (cnst expr)
                                         :else (vr (str expr))))]
                (do-parse (read-string input)))))

(def parseFunction (common-parse {'+      add
                                  '-      subtract
                                  '*      multiply
                                  '/      divide
                                  'negate negate
                                  'med    med
                                  'avg    avg} constant variable))

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
(def toStringInfix (method :toStringInfix))

(declare ZERO)
(def Constant (let [value (field :value)]
                (constructor (fn [this value] (assoc this :value value))
                             {:evaluate (fn [this _] (value this))
                              :diff     (fn [_ _] ZERO)
                              :toString       (fn [this] (format "%.1f" (double (value this))))
                              :toStringInfix  (fn [this] (format "%.1f" (double (value this))))})))
(def ZERO (Constant 0))
(def ONE (Constant 1))
(def E (Constant Math/E))

(def Variable (let [name (field :name)]
                (constructor (fn [this name] (assoc this :name name))
                             {:evaluate (fn [this vars] (get vars (name this)))
                              :diff     (fn [this d-name] (if (= d-name (name this))
                                                            ONE
                                                            ZERO))
                              :toString      (fn [this] (name this))
                              :toStringInfix (fn [this] (name this))})))

(def Op-proto (let [operands (field :operands)
                    name (field :name)
                    calc-func (field :calc-func)
                    diff-func (field :diff-func)]
                {:evaluate (fn [this vars] (apply (calc-func this) (mapv #(evaluate % vars) (operands this))))
                 :diff     (fn [this d-name] ((diff-func this) (operands this)  (mapv #(diff % d-name) (operands this))))
                 :toString (fn [this] (str "(" (name this) " "
                                           (clojure.string/join " " (mapv toString (operands this)))
                                           ")"))
                 :toStringInfix (fn [this] (str
                                             (if (== 1 (count (operands this)))
                                               (name this))
                                             "("
                                             (clojure.string/join (str " " (name this) " ") (mapv toStringInfix (operands this)))
                                             ")"))}))

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
                            (fn [[a & as] [da & d-as]] (let [b  (apply Multiply (apply vector as))
                                                             db (diff-mult (apply vector as)
                                                                           (apply vector d-as))]
                                                         (if (== 0 (count as))
                                                           (Divide (Negate da)
                                                                   (Multiply a a))
                                                           (Divide (Subtract (Multiply da b)
                                                                             (Multiply a db))
                                                                   (Multiply b b)))))))

(def Sum (make-operation "sum" + diff-sum))

(def Avg (make-operation "avg"
                         (fn [& args] (/ (double (apply + args)) (double (count args))))
                         (fn [args d-args] (apply Avg d-args))))
;------------------------------
(declare Pow)
(declare Log)

(def Pow (make-operation "**"
                         (fn [a b] (Math/pow a b))
                         (fn [[a b] [da db]] (Multiply
                                               (Add (Multiply db (Log E a))
                                                    (Divide (Multiply b da) a))
                                               (Pow a b)))))

(def Log (make-operation "//"
                         (fn [a b] (/ (Math/log (Math/abs b)) (Math/log (Math/abs a))))
                         (fn [[a b] [da db]] (let [LnA (Log E a)
                                                   LnB (Log E b)]
                                               (Divide (Subtract (Divide (Multiply db LnA)
                                                                         b)
                                                                 (Divide (Multiply da LnB)
                                                                         a))
                                                       (Multiply LnA LnA))))))
;-----------------------------

(def TOKEN_TO_OBJ {'**           Pow
                   (symbol "//") Log
                   '+            Add
                   '-            Subtract
                   '*            Multiply
                   '/            Divide
                   'sum          Sum
                   'avg          Avg
                   'negate       Negate})

(def parseObject (common-parse TOKEN_TO_OBJ Constant Variable))

;---------------------------------
;           HW 12
;---------------------------------


;---------------------------------
(defn -return [value tail] {:value value :tail tail})
(def -valid? boolean)
(def -value :value)
(def -tail :tail)
(defn _show [result]
  (if (-valid? result) (str "-> " (pr-str (-value result)) " | " (pr-str (apply str (-tail result))))
                       "!"))
(defn tabulate [parser inputs]
  (run! (fn [input] (printf "    %-10s %s\n" (pr-str input) (_show (parser input)))) inputs))
(defn _empty [value] (partial -return value))
(defn _char [p]
  (fn [[c & cs]]
    (if (and c (p c)) (-return c cs))))
(defn _map [f result]
  (if (-valid? result)
    (-return (f (-value result)) (-tail result))))
(defn _combine [f a b]
  (fn [str]
    (let [ar ((force a) str)]
      (if (-valid? ar)
        (_map (partial f (-value ar))
              ((force b) (-tail ar)))))))
(defn _either [a b]
  (fn [str]
    (let [ar ((force a) str)]
      (if (-valid? ar) ar ((force b) str)))))
(defn _parser [p]
  (fn [input]
    (-value ((_combine (fn [v _] v) p (_char #{\u0000})) (str input \u0000)))))
(defn +char [chars] (_char (set chars)))
(defn +char-not [chars] (_char (comp not (set chars))))
(defn +map [f parser] (comp (partial _map f) parser))
(def +parser _parser)
(def +ignore (partial +map (constantly 'ignore)))
(defn iconj [coll value]
  (if (= value 'ignore) coll (conj coll value)))
(defn +seq [& ps]
  (reduce (partial _combine iconj) (_empty []) ps))
(defn +seqf [f & ps] (+map (partial apply f) (apply +seq ps)))
(defn +seqn [n & ps] (apply +seqf (fn [& vs] (nth vs n)) ps))
(defn +or [p & ps]
  (reduce _either p ps))
(defn +opt [p]
  (+or p (_empty nil)))
(defn +star [p]
  (letfn [(rec [] (+or (+seqf cons p (delay (rec))) (_empty ())))] (rec)))
(defn +plus [p] (+seqf cons p (+star p)))
(def *space (+char " \t\n\r"))
(def *ws (+ignore (+star *space)))
(defn +str [p] (+map (partial apply str) p))
(def *digit (+char "0123456789"))
(def *all-chars (mapv char (range 32 128)))
(def *letter (+char (apply str (filter #(Character/isLetter %) *all-chars))))
;--------------------------------------------

(defn *string [x] (apply +seqf str (mapv #(+char (str %)) x)))

(defn +oper [op] (+map (comp (partial get TOKEN_TO_OBJ) symbol str) op))

(defn apply-left [val other] (if (empty? other)
                               val
                               (let [op (first (first other))
                                     next-val (second (first other))]
                                 (apply-left (op val next-val)
                                             (rest other)))))

(defn apply-right [val other] (if (empty? other)
                                val
                                (let [op (first (first other))
                                      next-val (second (first other))]
                                  (op val
                                      (apply-right next-val (rest other))))))

(def *number (+map read-string (+str (+plus *digit))))
(def *float (+map read-string (+seqf str
                                     (+opt (+char "-"))
                                     *number
                                     (+opt (+seqf str (+char ".") *number)))))

(declare *bracket)
(def *constant (+map Constant *float))
(def *variable (+map Variable (+str (+plus *letter))))
(def *prime (+or *constant *variable (delay *bracket)))

(defn make-unary-expr [oper next-lvl]
  (let [unary-expr (def expr  (+or (+seqf (fn [op val] (op val))
                                          *ws
                                          (+oper oper)
                                          *ws
                                          (delay expr))
                                   next-lvl))]
    unary-expr))

(defn make-bin-expr [apply-fun]
  (fn [oper next-lvl] (+seqf apply-fun *ws next-lvl *ws (+map vec
                                                              (+star (+seqf vector *ws (+oper oper) *ws next-lvl *ws))))))

(def expr-left (make-bin-expr apply-left))
(def expr-right (make-bin-expr apply-right))

(def *expr (expr-left (+char "+-")
           (expr-left (+char "*/")
           (expr-right (+or (*string "**")
                             (*string "//"))
           (make-unary-expr (*string "negate") *prime)))))

(def *bracket (+seqn 1 *ws (+char "(") *expr (+char ")") *ws))

(def parseObjectInfix (+parser *expr))