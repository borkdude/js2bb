(ns js-cljs.class-test
  (:require [clojure.test :refer [deftest testing]]
            [check.core :refer [check]]
            [check.mocks :refer [mocking]]
            [js-cljs.core :refer [parse-str] :as core]))

(deftest js-members
  (check (parse-str "a.b") => "(.-b a)")
  (check (parse-str "a.b(1)") => "(.b a 1)")
  (check (parse-str "a.b = 1") => "(aset a \"b\" 1)")
  (check (parse-str "(()=> {a.b = 1})()") => "((fn [] (aset a \"b\" 1)))"))

(deftest spread-op
  (check (parse-str "a(...b)") => "(apply a b)")
  (check (parse-str "a(h, ...b)") => "(apply a h b)"))
  ; (check (parse-str "function a(...b){}") => "(defn a [& b] (let [b (clj->js b)]))"))

(deftest classes
  (testing "instantiate class"
    (check (parse-str "class A {}")
           => "(modern/defclass A (constructor [this]))")

    (check (parse-str "class A extends B {}")
           => "(modern/defclass A (extends B) (constructor [this]))")

    (check (parse-str "class B {constructor(a) { a }}")
           => "(modern/defclass B (constructor [this a] a))")

    (check (parse-str "class B {constructor(a) {} foo(a, b) { a + b } bar() {}}")
           => "(modern/defclass B (constructor [this a]) Object (foo [this a b] (+ a b)) (bar [this]))"))

  (testing "getters and setters"
    (check (parse-str "class B { get a() { return 10 } }")
           => (str "(modern/defclass B (constructor [this])) "
                   "(.defineProperty js/Object (.-prototype B) \"a\" "
                   "#js {:get (fn [] (this-as this 10))})"))

    (check (parse-str "class B { set a(v) { v } }")
           => (str "(modern/defclass B (constructor [this])) "
                   "(.defineProperty js/Object (.-prototype B) \"a\" "
                   "#js {:set (fn [v] (this-as this v))})")))

  (testing "class expressions"
    (check (parse-str "const a = class B {}")
           => (str "(def a (modern/defclass B (constructor [this])))")))

  (testing "instantiating a class"
    (check (parse-str "new String(a)") => "(String. a)")))
