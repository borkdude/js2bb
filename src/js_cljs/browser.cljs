(ns js-cljs.browser
  (:require [js-cljs.core :refer [parse-str]]))

(defonce ^:private editors (atom []))

(defn- parse-elems [[^js js, ^js cljs]]
  (let [in (. js getValue)
        debug (atom nil)
        to-cljs (parse-str in {:zprint-opts {:style [:community]
                                             :parse {:interpose "\n\n"}
                                             :width 60
                                             :pair {:nl-separator? true}}
                               :format-opts {:debug debug}})]
    (. cljs setValue to-cljs)))

(defn ^:dev/after-load main []
  (swap! editors conj
         (js/CodeMirror.fromTextArea
          (js/document.querySelector "#js")
          #js {:mode "javascript"
               :tabSize 2
               :lineNumbers true}))

  (swap! editors conj
         (js/CodeMirror.fromTextArea
          (js/document.querySelector "#cljs")
          #js {:mode "clojure"
               :tabSize 2
               :lineNumbers true}))
  (parse-elems @editors)

  (-> @editors first (.on "changes" #(parse-elems @editors))))

(defn ^:dev/before-load remove-editors []
  (doseq [^js editor @editors]
    (. editor toTextArea))
  (swap! editors empty))
