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
