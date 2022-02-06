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
  (p/let [in (readlines)
          debug (atom nil)]
    (try
      (println (parse-str in {:zprint-opts {:style [:community]
                                            :parse {:interpose "\n\n"}
                                            :width 60
                                            :pair {:nl-separator? true}}
                              :format-opts {:debug debug}}))
      (catch :default e
        (println "Error parsing JS file")
        (println "Last line to parse:" (count (.split (subs in 0 (:start @debug)) "\n")))
        (println "\nException:\n" e)
        (js/process.exit 1)))))
