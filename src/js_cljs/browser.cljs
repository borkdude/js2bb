(ns js-cljs.browser
  (:require [js-cljs.core :refer [parse-str]]))

(defonce error (atom {}))
(defonce ^:private editors (atom []))

(defn- set-visibility [visible?]
  (aset (js/document.querySelector "div.error") "hidden" (not visible?)))
(set-visibility false)

(defn- make-error-str [{:keys [error debug]}]
  (cond-> (.-message error)
    debug (str "\n" debug)))
(defn- parse-elems [[^js js, ^js cljs]]
  (let [in (. js getValue)
        debug (atom nil)
        to-cljs (try
                  {:result (parse-str in {:zprint-opts {:style [:community]
                                                        :parse {:interpose "\n\n"}
                                                        :width 60
                                                        :pair {:nl-separator? true}}
                                          :format-opts {:debug debug}})}
                  (catch :default e {:error e :debug @debug}))]

    (set-visibility (:error to-cljs))
    (if-let [success (:result to-cljs)]
      (. cljs setValue success)
      (aset (js/document.querySelector "div.error")
            "innerText" (make-error-str to-cljs)))))

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

  (let [[^js editor] @editors]
    (.on editor "changes" #(parse-elems @editors))))

(defn ^:dev/before-load remove-editors []
  (doseq [^js editor @editors]
    (. editor toTextArea))
  (swap! editors empty))
