(ns js-cljs.destructuring-test
  (:require [clojure.test :refer [deftest testing]]
            [check.core :refer [check]]
            [js-cljs.core :refer [parse-str]]))

(deftest js-objects
  (check (parse-str "a={a: 10, b: 20}") => "(def a #js {:a 10 :b 20})")
  (check (parse-str "[1, 2]") => "#js [1 2]"))

(deftest destructuring-maps
  (check (parse-str "const {a} = {a: 10}")
         => "(def a (.-a #js {:a 10}))")

  (check (parse-str "const {a, b} = {a: 10}")
         => "(let [--cache #js {:a 10}] (def a (.-a --cache)) (def b (.-b --cache)))"))
