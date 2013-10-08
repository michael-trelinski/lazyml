(ns ml.vector)

(comment
  "vector manipulation stuff... let's try to keep it as lazy as possible...")

(defn interleave-fn
  ([f c1 c2]
     (lazy-seq
      (let [s1 (seq c1) s2 (seq c2)]
        (when (and s1 s2)
          (cons (f (first s1) (first s2)) 
                (interleave-fn f (rest s1) (rest s2)))))))
  ([f c1 c2 & colls] 
    (lazy-seq 
      (let [ss (map seq (conj colls c2 c1))]
        (when (every? identity ss)
          (concat (pmap first ss) (apply interleave-fn f (pmap rest ss))))))))

(defn eye
  ([n]
    (eye n 0))
  ([n i]
    (cond
      (< n 1) (empty seq)
      (= n 1) 1
      :else (lazy-seq
              (if (> i (dec n))
                (empty seq)
                (cons (concat (take i (cycle [0])) (cons 1 (take (- (dec n) i) (cycle [0])))) (eye n (inc i))))))))

(defn all-the-same-number
  ([n x y]
    (cond
      (< x 1) (empty seq)
      (and (= x 1) (= y x)) n
      (= y 1) (take x (cycle [n]))
      :else (pmap (fn [i] (take y (cycle [i]))) (take x (cycle [n])))))
  ([n x]
    (all-the-same-number n x x)))

(defn ones
  ([x]
    (ones x x))
  ([x y]
    (all-the-same-number 1 x y)))

(defn zeros
  ([x]
    (zeros 0 x x))
  ([x y]
    (all-the-same-number 0 x y)))

(defn is-listy?
  [x]
  (or (seq? x) (vector? x) (list? x)))

(defn is-matrix?
  [x]
  (and (is-listy? x) (is-listy? (first x))))

(defn size
  ([x]
    (cond
      (is-matrix? x)  [(count (first x)) (count x)]
      (is-listy? x) [(count x) 1]
      :else [1 1]))
  ([x offset]
    ((size x) offset)))

(defn is-left-scalar-and-right-is-matrix?
  [one two]
  (and (number? one) (is-matrix? two)))

(defn is-left-matrix-and-right-is-scalar?
  [one two]
  (and (number? two) (is-matrix? one)))

(defn matrix-multiply
  [a b]
  (cond
    (is-left-scalar-and-right-is-matrix? a b) (pmap (fn [mtx] (pmap (fn [x] (* x a)) mtx)) b)
    (is-left-matrix-and-right-is-scalar? a b) (pmap (fn [mtx] (pmap (fn [x] (* x b)) mtx)) a)
    (and (is-matrix? a) (not (is-matrix? b)) (is-listy? b)) (apply + (interleave-fn * (pmap first a) b))
    (and 
      (is-listy? a) (is-listy? b) (= (size a 1) (size b 0))) (let [b-range (range 0 (size b 1))]
                                                               (pmap 
                                                                 (fn [a-n]
                                                                   (vec 
                                                                     (pmap
                                                                       (fn [p] 
                                                                         (apply + (interleave-fn * a-n (pmap #(nth %1 p) b)))) b-range)
                                                                     )) a))
  :else (throw (new Exception (str "unexpected operation for matrix-multiply " (type a) "/" (size a) " X " (type b) "/" (size b))))))
  
