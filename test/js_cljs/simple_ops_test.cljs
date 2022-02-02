(ns js-cljs.simple-ops-test
  (:require [clojure.test :refer [deftest testing]]
            [check.core :refer [check]]
            [js-cljs.core :refer [parse-str]]))

(deftest operators
  (testing "operators"
    (check (parse-str "1+2") => "(+ 1 2)")
    (check (parse-str "1-2") => "(- 1 2)"))

  (testing "comparision"
    (check (parse-str "!a") => "(not a)")
    (check (parse-str "a && b") => "(and a b)")
    (check (parse-str "a || b") => "(or a b)")
    (check (parse-str "a == b") => "(= a b)")
    (check (parse-str "a === b") => "(= a b)")
    (check (parse-str "a != b") => "(not= a b)")
    (check (parse-str "a !== b") => "(not= a b)")
    (check (parse-str "a < b") => "(< a b)")))

(deftest conditionals
  (testing "if-then-else"
    (check (parse-str "if(a) { b } else { c }") => "(if a b c)")
    (check (parse-str "if(true) { 1+2;2+3 } else { false }")
           => "(if true (do (+ 1 2) (+ 2 3)) false)"))

  (testing "when"
    (check (parse-str "if(false) { 1; true }") => "(when false 1 true)")))

; (deftest multi-operators
;   (testing "comparision"
;     (check (parse-str "!(1 === 2)") => "(not (= 1 2))")))
;     (check (parse-str "a && b") => "(and a b)")
;     (check (parse-str "a || b") => "(or a b)")
;     (check (parse-str "a == b") => "(= a b)")
;     (check (parse-str "a === b") => "(= a b)")
;     (check (parse-str "a != b") => "(not= a b)")
;     (check (parse-str "a !== b") => "(not= a b)")
;     (check (parse-str "a < b") => "(< a b)")))
;
; (deftest conditionals
;   (testing "if-then-else"
;     (check (parse-str "if(a) { b } else { c }") => "(if a b c)"))
;
;   (testing "when"
;     (check (parse-str "if(a) { b }") => "(when a b)")))
