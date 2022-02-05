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
    (check (parse-str "if(false) { 1; true }") => "(when false 1 true)"))

  (testing "case"
    (check (parse-str "switch(a) { case 1: f(); break; default: g()}")
           => "(case a 1 (f) (g))")))

(deftest for-loops
  (testing "for ;;"
    (check (parse-str "for(let i=0; i < 10; i++) { a(i) }")
           => "(let [i 0] (while (< i 10) (a i) (js* \"~{}++\" i)))"))

  (testing "for...of"
    (check (parse-str "for(var i of b) { a(i) }") => "(doseq [i b] (a i))"))
    ; (check (parse-str "for(var {i} of b) { a(i) }") => "(doseq [i b] (a i))")))

  (testing "for...in"
    (check (parse-str "for(var i in b) { a(i) }")
           => "(doseq [i (js/Object.keys b)] (a i))")))
    ; (check (parse-str "for(var {i} in b) { a(i) }")
    ;        => "(doseq [i (js/Object.keys b)] (a i))")))

(deftest while-loops
  (check (parse-str "while(true) {a(1)}") => "(while true (a 1))"))

(deftest template-literals
  (testing "interpolated-strings"
    (check (parse-str "`a${b}c${d}e${f}`")
           => "(modern/js-template \"a\" b \"c\" d \"e\" f)")
    (check (parse-str "`a${b}c${d}e${f}g`")
           => "(modern/js-template \"a\" b \"c\" d \"e\" f \"g\")")
    (check (parse-str "foo`a${b}c`")
           => "(modern/js-template foo \"a\" b \"c\")")))

; (deftest flow-keywords
;   (check (parse-str "continue") => "(js* \"continue\")"))
