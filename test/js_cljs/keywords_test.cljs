(ns js-cljs.keywords-test
  (:require [clojure.test :refer [deftest testing]]
            [check.core :refer [check]]
            [js-cljs.core :refer [parse-str] :as core]))

(deftest conditionals
  (testing "if-then-else"
    (check (parse-str "if(a) { b } else { c }") => "(if a b c)")
    (check (parse-str "if(true) { 1+2;2+3 } else { false }")
           => "(if true (do (+ 1 2) (+ 2 3)) false)")

    (check (parse-str "true ? 1 : 2") => "(if true 1 2)"))

  (testing "when"
    (check (parse-str "if(false) { 1; true }") => "(when false 1 true)")))

(deftest for-loops
  (testing "for...of"
    (check (parse-str "for(var i of b) { a(i) }") => "(doseq [i b] (a i))")))
    ; (check (parse-str "for(var {i} of b) { a(i) }") => "(doseq [i b] (a i))")))
