(ns js-cljs.all-tests
  (:require [clojure.test :as test]

            [js-cljs.simple-ops-test]))

(defn ^:dev/after-load run-tests []
  (test/run-all-tests #"js-cljs.*-test"))
