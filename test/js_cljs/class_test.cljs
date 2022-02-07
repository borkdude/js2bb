(ns js-cljs.class-test
  (:require [clojure.test :refer [deftest testing]]
            [check.core :refer [check]]
            [check.mocks :refer [mocking]]
            [js-cljs.core :refer [parse-str] :as core]))

(deftest js-members
  (testing "attributes"
    (check (parse-str "a.b") => "(.-b a)")
    (check (parse-str "a[b]") => "(aget a b)")
    (check (parse-str "a[0]") => "(nth a 0)")
    (check (parse-str "a[\"1b\"]") => "(aget a \"1b\")"))
  
  (testing "methods"
    (check (parse-str "a.b(1)") => "(.b a 1)")
    (check (parse-str "a.b1(2)") => "(.b1 a 2)"))

  (testing "setting"
    (check (parse-str "a.b = 1") => "(aset a \"b\" 1)")
    (check (parse-str "a[b] = 1") => "(aset a b 1)")
    (check (parse-str "a.b[c] = 1") => "(aset (.-b a) c 1)")
    (check (parse-str "(()=> {a.b = 1})()") => "((fn [] (aset a \"b\" 1)))")

    (check (parse-str "a[b-1]") => "(aget a (- b 1))")
    (check (parse-str "a[b-1] = 1") => "(aset a (- b 1) 1)"))

  (testing "inside other functions"
    (check (parse-str "f(a.b)") => "(f (.-b a))")))

(deftest spread-op
  (check (parse-str "a(...b)") => "(apply a b)")
  (check (parse-str "a(h, ...b)") => "(apply a h b)")
  (check (parse-str "a(h, ...b, c)") => "(apply a h (concat b [c]))")
  ; (check (parse-str "function a(...b){}") => "(defn a [& b] (let [b (clj->js b)]))")

  (check (parse-str "function a(...b) {}")
         => "(defn a [& b] )"))

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
     (check (parse-str "new String(a)") => "(String. a)"))

  (testing "destructuring on methods"
    (mocking
     (core/random-identifier) => "--c"
     ---
     (check (parse-str "class A { constructor({a}) { a }}")
            => "(modern/defclass A (constructor [this --c] (let [a (.-a --c)] a)))"))))

(deftest this
  (testing "this outside classes"
    (check (parse-str "this.f") => "(.-f (js* \"this\"))"))

  (testing "and inside a class"
    (check (parse-str "class A { constructor() { this.a }}")
           => "(modern/defclass A (constructor [this] (.-a this)))")))
