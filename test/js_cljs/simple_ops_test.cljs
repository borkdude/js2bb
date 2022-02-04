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
    (check (parse-str "a < b") => "(< a b)"))

  (testing "fn call"
    (check (parse-str "foo(a, b)") => "(foo a b)")))

(deftest conditionals
  (testing "if-then-else"
    (check (parse-str "if(a) { b } else { c }") => "(if a b c)")
    (check (parse-str "if(true) { 1+2;2+3 } else { false }")
           => "(if true (do (+ 1 2) (+ 2 3)) false)")

    (check (parse-str "true ? 1 : 2") => "(if true 1 2)"))

  (testing "when"
    (check (parse-str "if(false) { 1; true }") => "(when false 1 true)")))

(deftest functions
  (testing "named functions"
    (check (parse-str "function lol (a, b) { return a + b}")
           => "(defn lol [a b] (+ a b))"))

  (testing "anon functions"
    (check (parse-str "(function(a, b) { return a + b})(1, 2)")
           => "((fn [a b] (+ a b)) 1 2)")
    (check (parse-str "(function f (a, b) { return a + b})(1, 2)")
           => "((fn f [a b] (+ a b)) 1 2)")
    (check (parse-str "(function lol (a, b) { return a + b})(1, 2)")
           => "((fn lol [a b] (+ a b)) 1 2)")
    (check (parse-str "((a, b) => a+b)(1, 2)")
           => "((fn [a b] (+ a b)) 1 2)")
    (check (parse-str "((a, b) => {a+b})(1, 2)")
           => "((fn [a b] (+ a b)) 1 2)"))

  (testing "default options"
    (check (parse-str "function a(b, c=1){c}")
           => "(defn a [b c] (let [c (if (undefined? c) 1 c)] c))"))

  (testing "alternate definition of functions"
    (check (parse-str "const a = (b) => b") => "(defn a [b] b)")))

(deftest vars
  (testing "global vars"
    (check (parse-str "a = 10") => "(def a 10)")
    (check (parse-str "const a = 10") => "(def a 10)")
    (check (parse-str "let a = 10") => "(def a 10)")
    (check (parse-str "var a = 10") => "(def a 10)"))

  (testing "multiple global vars"
    (check (parse-str "const a = 10, b=20") => "(def a 10) (def b 20)"))

  (testing "local vars"
    (check (parse-str "function a() { const a=1,b=2; a+b }")
           => "(defn a [] (let [a 1 b 2] (+ a b)))")))

(deftest multiple-commands
  (check (parse-str "a=1;b=2") => "(def a 1)\n(def b 2)"))
