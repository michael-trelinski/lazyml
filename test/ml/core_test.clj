(ns ml.core-test
  (:use clojure.test
        ml.core
        ml.vector
        ml.files
        regression.linear))

(deftest ones-test
  (testing "ones two three"
    (is (= '((1 1 1) (1 1 1)) (ones 2 3))))
  (testing "ones single two"
    (is (= '((1 1) (1 1)) (ones 2))))
  (testing "ones one"
    (is (= 1 (ones 1))))
  (testing "ones zero"
    (is (nil? (ones 0))))
  (testing "eyes three"
    (is (= (eye 3) '((1 0 0) (0 1 0) (0 0 1)))))
  (testing "eyes two"
    (is (= (eye 2) '((1 0) (0 1)))))
  (testing "eyes one"
    (is (= (eye 1) 1)))
  (testing "eyes zero"
    (is (nil? (eye 0)))))

(deftest matrix-math
  (testing "matrix multiplication"
    (is (= [[30 36 42] [12 15 18] [78 99 120]]
          (ml.vector/matrix-multiply [[1 2 3] [1 1 1] [8 7 6]] [[1 2 3] [4 5 6] [7 8 9]])))))

(deftest linear-regression-test
  (testing
    (def data (csv-load "./ex1data1.txt"))
    (def Xs (pmap #(vec (list 1 (Double/parseDouble (first %1)))) data))
    (def ys (pmap #(Double/parseDouble (second %1)) data))
    (def thetas (zeros 2 1))
    (def max-iterations 1500)
    (def learning-rate 0.01)
      
    (def new-theta-tuple (gradient-descent Xs ys thetas learning-rate max-iterations))
  
    (def new-theta (first new-theta-tuple))
    (testing "thetas"
             (is (= new-theta [-3.6302914394043593 1.166362350335582])))
    (testing "prediction"
             (is (= (* (predict [[1] [3.5]] new-theta) 10000) 4519.767867701776)))))

(time
  (do
    (ones-test)
    (matrix-math)
    (linear-regression-test)))

