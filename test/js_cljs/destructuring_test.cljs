(ns js-cljs.destructuring-test
  (:require [clojure.test :refer [deftest testing]]
            [check.core :refer [check]]
            [check.mocks :refer [mocking]]
            [js-cljs.core :refer [parse-str] :as core]))

(deftest js-objects
  (check (parse-str "a={a: 10, b: 20}") => "(def a #js {:a 10 :b 20})")
  (check (parse-str "[1, 2]") => "#js [1 2]"))

(deftest js-arrays
  (check (parse-str "a[0]") => "(nth a 0)")
  (check (parse-str "a[0] = 1") => "(aset a 0 1)"))

(deftest destructuring-maps-on-def
  (testing "simple destructuring"
    (check (parse-str "const {a} = {a: 10}")
           => "(def a (.-a #js {:a 10}))"))

  (testing "multiple destructuring"
    (mocking
     (core/random-identifier) => "--cache"
     ---
     (check (parse-str "const {a, b} = {a: 10}")
            => "(let [--cache #js {:a 10}] (def a (.-a --cache)) (def b (.-b --cache)))"))))

; (deftest destructuring-vector-on-def
;   (mocking
;    (core/random-identifier) => "--cache"
;    ---
;    (testing "simple destructuring"
;      (check (parse-str "const [a, b] = c")
;             => "(let [--cache c] (def a (nth --cache 0)) (def b (nth --cache 1)))"))))

(deftest destructuring-vector-on-function
  (mocking
   (core/random-identifier) => "--cache"
   ---
   (testing "simple destructuring"
     (check (parse-str "function f([a, b]) {}")
            => "(defn f [[a b]] )"))))

(deftest destructuring-maps-on-function
  (mocking
   (core/random-identifier) => "--cache"
   ---
   (check (parse-str "function f({a, b}) { a }")
          => "(defn f [--cache] (let [a (.-a --cache) b (.-b --cache)] a))")))
  ;
  ; (check (parse-str "const {a, b} = {a: 10}")
  ;        => "(let [--cache #js {:a 10}] (def a (.-a --cache)) (def b (.-b --cache)))"))
