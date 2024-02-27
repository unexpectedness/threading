(ns threading.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [threading.core :refer [<- <<-
                                    >- >>-
                                    >-> >->>
                                    >>-> >>->>
                                    dotos doto-> doto->>
                                    if-> if->>
                                    if-not-> if-not->>
                                    when->
                                    when-not->
                                    pp-> pp->>
                                    and-> and->>
                                    or-> or->>
                                    not-> not->>
                                    map-> mapv-> mapcat->
                                    map-keys-> map-keys->>
                                    map-vals-> map-vals->>
                                    juxt-> juxt->>
                                    juxtm-> juxtm->>
                                    let-> let->>
                                    when-let-> when-let->>
                                    binding-> binding->>
                                    -o -oo o- oo-
                                    >-args >>-args
                                    filter-> filter->>]]))

(deftest test-<-
  (is (= 124 (->  :x (<-  123) inc)))
  (is (= 124 (->> :x (<<- 123) inc))))

(deftest test->>-
  (is (= 5
         (-> 100 (/ 10 2))
         (-> (/ 10 2) (>-> 100))
         (-> (/ 10 2) (>- (-> 100)))))
  (is (= 5
         (->> 2 (/ 100 10))
         (-> (/ 100 10) (>->> 2))
         (-> (/ 100 10) (>- (->> 2)))))
  (is (= 5
         (-> 2 (>- (/ 100 10))))))

(deftest test->>-
  (is (= 5 (->  100 (/ 10 2))
         (->> 100 (>>-> (/ 10 2)))
         (->> 100 (>>- (->  (/ 10 2))))))
  (is (= 5 (->> 10  (/ 100 2))
         (->> 10  (>>->> (/ 100 2)))
         (->> 10  (>>- (->> (/ 100 2))))))
  (is (= 5 (->> 100 (>>- (/ 10 2))))))


(deftest test-doto->
  (is (= 1 (doto-> 1 inc)))
  (is (= "1 a\n"
         (with-out-str (doto-> 1 (println "a")))))
  (testing "with doto->>"
    (is (= 1 (doto->> 1 inc)))
    (is (= "a 1\n"
           (with-out-str (doto->> 1 (println "a")))))))

(deftest test-dotos
  (dotos (with-out-str
           (dotos (dotos 123
                         (println "yo")
                         (-> println)
                         (println "yes"))
                  (as-> $ (is (= $ 123)))))
         (as-> $ (is (= $ "yo\n123\nyes\n")))))

(deftest test-if->
  (is (= 2     (if-> 1  number? inc (str "a")))
      (= "a:x" (if-> :x number? inc (str "a"))))
  (testing "with if->>"
    (is (= 2     (if->> 1  number? inc (str "a")))
        (= ":xa" (if->> :x number? inc (str "a")))))
  (testing "assserting a missing second case is equivalent to 'identity'"
    (testing "when the predicate succeeds"
      (is (= 2  (if-> 1  number? inc))))
    (testing "when the predicate fails"
      (is (= :x (if-> :x number? inc))))))

(defn- parse-int [s]
  #?(:clj  (Integer/parseInt s)
     :cljs (js/parseInt      s)))

(deftest test-if-not->
  (is (= "1a"  (if-not-> 1  number? inc (str "a")))
      (= "a:x" (if-not-> :x number? (str "a") inc)))
  (testing "with if-not->>"
    (is (= "a1"  (if-not->> 1  number? inc (str "a")))
        (= ":xa" (if-not->> :x number? (str "a") inc))))
  (testing "assserting a missing second case is equivalent to 'identity'"
    (testing "when the predicate succeeds"
      (is (= 2    (if-not-> "1" number? (-> parse-int inc)))))
    (testing "when the predicate fails"
      (is (= ":x" (if-not-> :x number? str))))))

(deftest test-when->
  (is (= "125"  (-> 123 (when-> number? inc inc) str)))
  (is (= ":x" (-> :x  (when-> number? inc inc) str))))

(deftest test-when-not->
  (is (= "123" (-> 123 (when-not-> number? inc inc) str)))
  (is (= :x    (-> :x  (when-not-> number? name keyword)))))

(deftest test-pp->
  (is (= (-> 1 (/ 2) (/ 2) (/ 2))
         (pp-> 1 (/ 2) (/ 2) (/ 2))))
  (testing "with pp->>"
    (is (= (->> 1 (/ 2) (/ 2) (/ 2))
           (pp->> 1 (/ 2) (/ 2) (/ 2))))))

(deftest test-and->
  (is (true?  (and->  1 number? (> 0))))
  (is (false? (and->> 1 number? (> 0)))))

(deftest test-or->
  (is (true? (or->  1 number? (> 0))))
  (is (true? (or->> 1 number? (> 0)))))

(deftest test-not->
  (is (false? (not->  1  inc  number?)))
  (is (true?  (not->> :x name number?))))

(deftest test-map->
  (is (= [3 3 3]    (map-> (repeat 3 1) inc inc)))
  (is (= [-1 -1 -1] (map-> (repeat 3 1) -))))

(deftest test-mapv->
  (is (-> (mapv-> (repeat 3 1) inc inc)
          (and->> (= [3 3 3])
                  vector?)))
  (is (-> (mapv-> (repeat 3 1) -)
          (and->> (= [-1 -1 -1])
                  vector?))))

(deftest test-mapcat->
  (is (= [3 3 3]    (mapcat-> (vec (repeat 3 [1]))
                              (update 0 inc) (update 0 inc))))
  (is (= [-1 -1 -1] (mapcat-> (vec (repeat 3 [1]))
                              (update 0 -)))))

(deftest test-map-keys->
  (is (= {"ax" 1} (map-keys->  {:a 1} name (str "x"))))
  (is (= {"xa" 1} (map-keys->> {:a 1} name (str "x"))))
  (is (= [["a" 1] ["b" 2]]
         (map-keys->  [["a" 1] ["b" 2]] name)
         (map-keys-> '(["a" 1] ["b" 2]) name))))

(deftest test-map-vals->
  (is (= {:a 0.5} (map-vals->  {:a 1} (/ 2) float)))
  (is (= {:a 2}   (map-vals->> {:a 1} (/ 2))))
  (= [[:a 2] [:b 3]]
     (map-vals->  [[:a 1] [:b 2]] inc)
     (map-vals-> '([:a 1] [:b 2]) inc)))

(deftest test-filter->
  (is (= [1 3 5 7 9] (filter->  (range 10) odd?)))
  (is (= [2]         (filter->> (range 10) (- 2) zero?))))

(deftest test-juxt->
  (is (= [0.5 0 -1] (-> 1 (juxt->  (-> (/ 2) float) dec -))))
  (is (= [2   0 -1] (-> 1 (juxt->> (/ 2) dec -)))))

(deftest test-juxtm->
  (is (= {:inc 2 :dec 0} (juxtm->  1 :inc inc   :dec dec)))
  (is (= {:one 1 :two 2} (juxtm->> 1 :one (/ 1) :two (/ 2)))))

(deftest test-let->
  (is (= 1.5 (-> 1 (let->  [a (/ 2)] (-> (+ a) float)))))
  (is (= 3   (-> 1 (let->> [a (/ 2)] (+ a)))))
  (is (= 6         (let-> 1 [a inc
                             b (* a 2)]
                          (+ (inc b)))))
  (let [x (atom 0)]
    (is (= 3 (-> 1 (let-> [_ (<- (swap! x inc))
                           _ (<- (swap! x inc))]
                          inc
                          inc))))
    (is (= 2 @x))))

(deftest test-when-let->
  (is (= 2      (when-let->  {:a 1} [a :a] (-> :a (+ a)))))
  (is (= {:a 1} (when-let->  {:a 1} [b :b] (-> :a (+ b)))))
  (let [multiple (fn [n x]
                   (let [ret (/ x n)]
                     (when (integer? ret) ret)))]
    (is (= 0.5 (when-let->> 4 [a (multiple 2)] (/ a) float)))
    (is (= 4   (when-let->> 4 [a (multiple 3)] (/ a) float)))))

(def ^:dynamic *a*)
(deftest test-binding->
  (is (= 1.5 (-> 1 (binding->  [*a* (/ 2)] (-> (+ *a*) float)))))
  (is (= 3   (-> 1 (binding->> [*a* (/ 2)] (+ *a*)))))
  (is (= 5 (let->> 1 [a inc
                      b (* a 2)]
                   (/ (inc b)))))
  (let [x (atom 0)]
    (is (= 4 (-> 1 (binding-> [*a* (<- (swap! x inc))
                               *a* (<- (swap! x inc))]
                              (<- (swap! x inc))
                              (<- (swap! x inc))))))))

(deftest test--o-&-o
  (testing "o-"
    (is (= 4 (-> 1 (o- (+ 1 (-o inc))))))
    (is (= 3 (-> 1 (o- (+ 1 (-o))))))
    (is (= 4 (-> 1 (o- (+ 1 (-oo (/ 2)))))))
    (is (= 3 (-> 1 (o- (+ 1 (-oo)))))))
  (testing "oo-"
    (is (= 4 (->> 1 (oo- (+ 1 (-o inc))))))
    (is (= 3 (->> 1 (oo- (+ 1 (-o))))))
    (is (= 4 (->> 1 (oo- (+ 1 (-oo (/ 2)))))))
    (is (= 3 (->> 1 (oo- (+ 1 (-oo))))))))

(deftest test-->args
  (is (= 4.5 (->  1 (-> (>-args  (->  (+ inc inc (/ 2))))
                        float))))
  (is (= 6   (->> 1 (>>-args (->> (+ inc inc (/ 2))))))))
