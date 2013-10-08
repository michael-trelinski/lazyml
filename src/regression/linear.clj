(ns regression.linear
  (:use ml.files ml.vector))

; linear regression

(defn compute-cost
  [X y theta]
  (let [m (size y 0)
        sum (apply 
              +
              (pmap
                (fn [y-i-and-x-i]
                  (let [h-theta-x (apply 
                                    + 
                                    (first theta)
                                    (pmap
                                      (fn [theta-n-and-x-i-n]
                                        (* (first theta-n-and-x-i-n) (second theta-n-and-x-i-n)))
                                      (interleave-fn list (rest theta) (rest (second y-i-and-x-i)))))]
                    (Math/pow (- h-theta-x (first y-i-and-x-i)) 2)))
                (interleave-fn list y X)))]
    (* (/ 1 (* 2 m)) sum)))

(defn gradient-descent
  ([X y theta alpha iterations j-history]
    (lazy-seq
      (if (<= iterations 0) 
        [theta j-history]
        (let [m (size y 0)
              temp (seq theta)]
          (let [sums (pmap
                       (fn [j] (apply +  
                                      (map 
                                        (fn [item-i] (* (- (apply + 
                                                                  (first theta)
                                                                  (pmap
                                                                    (fn [theta-and-x-i] (* (first theta-and-x-i) (second theta-and-x-i)))
                                                                    (interleave-fn list (rest theta) (rest (second item-i)))))                              
                                                           (first item-i))                            
                                                        (nth (second item-i) j)))
                                        (interleave-fn list y X)))) (range (size theta 0)))
                new-thetas (pmap 
                             (fn [temp-theta-and-sum] 
                               (- (first temp-theta-and-sum) (* alpha (/ 1 m) (second temp-theta-and-sum)))) (interleave-fn list temp sums))]
            (gradient-descent X y new-thetas alpha (dec iterations) (cons (compute-cost X y new-thetas)) j-history))))))                    
  ([X y theta alpha iterations]
    (gradient-descent X y theta alpha iterations (empty seq))))

(defn predict 
  [x theta] 
  (matrix-multiply x theta))
