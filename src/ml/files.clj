(ns ml.files)

(defn csv-load
  [f]
  (pmap 
    (fn [line] (vec (.split (str line) ","))) 
    (line-seq (clojure.java.io/reader f))))