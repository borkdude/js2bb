(ns js-cljs.node
  (:require [js-cljs.core :refer [parse-str]]
            [promesa.core :as p]))

(defn- readlines []
  (let [p (p/deferred)
        buffer (atom "")]
    (doto (.-stdin js/process)
          (.resume)
          (.on "data" #(swap! buffer str %))
          (.on "end" #(p/resolve! p @buffer)))
    p))

(def ^:private current-filename js/__filename)

(defn main [ & args]
  (p/let [in (readlines)]
    (println (parse-str in {:isHangEnabled true}))))
