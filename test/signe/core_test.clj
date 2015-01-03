(ns signe.core-test
  (:require [clojure.test :refer :all]
            [signe.core :refer :all]))

(deftest is-root?-test
  (testing "root-test"
      (is (is-root? (make-root)))))

(deftest call-test
  (testing "call-test"
      (is (= '(1 2) (:args (call + 1 2))))))

(deftest make-accessor-list-test
  (testing "accessor-test"
    (is (= [] (make-accessor-list (make-root))))
    (is (= 1 (count (make-accessor-list (call + (make-root) 1)))))))

(deftest apply-accessor-test
  (testing "apply-accessor-test"
    (is (= 120 (let [fnc (call + 1 (make-root))
                     accs (make-accessor-list fnc)
                     acc (first accs)]
                 (assert (Accessor? acc))
                 (apply-accessor acc 119))))))

(deftest insert-test
  (testing "insert-test"
    (is (AccessorMap? (insert nil [] (fn [] nil))))
    (let [testobj (insert
                   (insert nil [] (fn [] 119))
                   [:kattskit]
                   (fn [] 120))]
      (is (AccessorMap? testobj))
      (is (contains? (:map testobj) :kattskit))
      (is (= 119 ((first (:update-functions testobj)))))
      (is (= 120 ((first (:update-functions (:kattskit (:map testobj))))))))))

(deftest keys-test
  (testing "keys"
    (is (= '(:rulle) (keys (assoc {} :rulle 1))))))

(deftest make-monitor-test
  (testing "make-monitor-test"
    (is (Monitor? (make-monitor (atom 0))))))

(deftest call-updaters-test
  (let [old-model (assoc {} :age 19)
        new-model (assoc {} :age 20)
        new-model-nil nil
        a0 (make-empty-accessor-map)
        get-age (fn [x] (:age x))
        acc-vec (make-accessor-list (call get-age obj))
        acc-vec2 (make-accessor-list (call even? (call get-age obj)))
        a1 (insert
            a0
            acc-vec
            (fn [old-value new-value] (println
                             (str
                              "Successfully called! New value is "
                              new-value))
              119))
        a2 (do
             ;(println acc-vec2)
             (insert a1 acc-vec2
                   (fn [a x] (println (str "The new value is "
                                         (if x "even" "odd"))) true)))
        updated-a1 (call-updaters-old a1 old-model new-model)]
    (testing "call-updaters-test"
      (is (= a1 updated-a1))
      (is (let [result-map (call-updaters-old a1 old-model new-model-nil)]
             (empty? (:map result-map))))
      (is (= a2 (call-updaters-old a2 old-model new-model)))
      (is (nil? (call-updaters-old nil 1 2)))
      (is (nil? (call-updaters-old nil 1 1)))
      (is (nil? (call-updaters-old (make-empty-accessor-map) 1 1))))))
        
(deftest remove-key-test
  (testing "remove-key-test"
    (is (= {:malte 3} (remove-keys [:rulle :signe] {:rulle 1, :signe 2, :malte 3})))))

      
                       
