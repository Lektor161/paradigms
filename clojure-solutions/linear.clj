(defn ten-rank [tensor] (loop [ans 0 ten tensor]
                          (if (number? ten)
                            ans
                            (recur (+ 1 ans) (first ten)))))

(defn ten-shape [tensor] (loop [ls () ten tensor]
                           (if (number? ten)
                             ls
                             (recur (conj ls (count ten)) (first ten)))))

(defn check-tens [tensors] (apply = (mapv ten-shape tensors)))

(defn tensor? [tensor] (or (number? tensor)
                           (and (vector? tensor)
                                (or (every? number? tensor)
                                    (and (every? tensor? tensor)
                                         (check-tens tensor))))))

(defn abstract-ten-op [op rank] (fn [& tensors]
                                  {:pre  [(every? tensor? tensors)
                                          (check-tens tensors)
                                          (or (nil? rank)
                                              (== rank (ten-rank (first tensors))))]
                                   :post [(tensor? %)
                                          (check-tens (vector % (first tensors)))]}
                                  (letfn [(do-op [& tens] (if (every? number? tens)
                                                            (apply op tens)
                                                            (apply mapv (fn [& sub-tens] (apply do-op sub-tens)) tens)))]
                                    (apply do-op tensors))))

(defn abstract-mul-scalar [rank] (fn [tensor & scalars]
                                   {:pre  [(tensor? tensor)
                                           (== rank (ten-rank tensor))]
                                    :post [(tensor? %)
                                           (check-tens (vector % tensor))]}
                                   (let [scalars-mul (apply * scalars)
                                         do-mul (fn do-mul [ten] (if (number? ten)
                                                                   (* ten scalars-mul)
                                                                   (mapv do-mul ten)))]
                                     (do-mul tensor))))
;-----------------------------------------------------------------------------------------------------------------------
(defn ten-op [op] (abstract-ten-op op nil))
(def t+ (ten-op +))
(def t- (ten-op -))
(def t* (ten-op *))
;-----------------------------------------------------------------------------------------------------------------------
(defn num-vec? [vec] (and (tensor? vec)
                      (== 1 (ten-rank vec))))

(defn check-vecs [vecs] (and (every? num-vec? vecs)
                        (check-tens vecs)))

(defn vec-op [op] (abstract-ten-op op 1))
(def v+ (vec-op +))
(def v- (vec-op -))
(def v* (vec-op *))
(def v*s (abstract-mul-scalar 1))

(defn scalar [& vectors]
  {:pre  [(check-vecs vectors)]
   :post [(number? %)]}
  (apply + (apply v* vectors)))

(defn vect [& vectors]
  {:pre  [(check-vecs vectors)]
   :post [(check-vecs (vector % (first vectors)))]}
  (reduce (fn [v1 v2] [(- (* (v1 1) (v2 2)) (* (v1 2) (v2 1)))
                       (- (* (v1 2) (v2 0)) (* (v1 0) (v2 2)))
                       (- (* (v1 0) (v2 1)) (* (v1 1) (v2 0)))]) vectors))
;-----------------------------------------------------------------------------------------------------------------------
(defn mat? [mat] (and (tensor? mat)
                      (== 2 (ten-rank mat))))

(defn check-mats [& mats] (and (every? mat? mats)
                                (check-tens mats)))

(defn mat-op [op] (abstract-ten-op op 2))
(def m+ (mat-op +))
(def m- (mat-op -))
(def m* (mat-op *))
(def m*s (abstract-mul-scalar 2))
(defn m*v [matrix vector]
  {:pre [(mat? matrix)
         (== (count (first matrix)) (count vector))]
   :post [(num-vec? %)
          (== (count %) (count matrix))]}
  (mapv (fn [v] (scalar v vector)) matrix))

(defn transpose [matrix]
  {:pre [(mat? matrix)]
   :post [(mat? %)
          (== (count %) (count (first matrix)))
          (== (count (first %)) (count matrix))]}
  (apply mapv vector matrix))

(defn m*m [& matrices]
  {:pre  [(every? mat? matrices)]
   :post [(mat? %)
          (== (count %) (count (first matrices)))
          (== (count (first %)) (count (first (last matrices))))]}
  (letfn [(m*m-bin [A B]
            (mapv (fn [v] (m*v (transpose B) v)) A))]
    (reduce m*m-bin matrices)))