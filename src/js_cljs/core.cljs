(ns js-cljs.core
  (:require ["acorn" :refer [parse]]
            ["zprint-clj" :as zprint]
            [clojure.string :as str]))

(defmulti parse-frag (fn [step _] (:type step)))

(defn- block [step state]
  (let [ops (map #(parse-frag % state) (:body step))
        ; body (str/join " " (map #(parse-frag % state) (:body step)))
        body (->> (:body step)
                  (map #(parse-frag % state))
                  (remove nil?)
                  (str/join " "))
        locals (:locals state)]
    (cond
      (and locals (seq @locals)) (str "(let [" (str/join " " (mapcat identity @locals)) "] " body ")")
      (-> state :single? not (or (-> ops count (= 1)))) body
      :else (str "(do " body ")"))))

(defmethod parse-frag "Program" [step state]
  (block step (assoc state :root? true)))

(defmethod parse-frag "BlockStatement" [step state]
  (block step (assoc state :root? false :locals (atom []))))

(defmethod parse-frag "ExpressionStatement" [step state]
  (parse-frag (:expression step) state))

(defn- get-operator [operator]
  (case operator
    "&&" "and"
    "||" "or"
    "==" "="
    "===" "="
    "!=" "not="
    "!==" "not="
    "!" "not"
    operator))

(defn- binary-exp [{:keys [left right operator]} state]
  (let [operator (get-operator operator)]
    (str "("
         operator
         " " (parse-frag left (assoc state :single? true))
         " " (parse-frag right (assoc state :single? true))
         ")")))

(defmethod parse-frag "UnaryExpression" [{:keys [operator argument]} state]
  (let [operator (get-operator operator)]
    (str "(" operator " " (parse-frag argument (assoc state :single? true)) ")")))

(defmethod parse-frag "BinaryExpression" [step state] (binary-exp step state))
(defmethod parse-frag "LogicalExpression" [step state] (binary-exp step state))

(defmethod parse-frag "Literal" [{:keys [value]} _] value)
(defmethod parse-frag "Identifier" [{:keys [name]} _] name)
(defmethod parse-frag "CallExpression" [{:keys [callee arguments]} state]
  (let [callee (parse-frag callee (assoc state :single? true :calling? true))
        args (map #(parse-frag % (assoc state :single? true)) arguments)]
    (if (string? callee)
      (str "(" (->> args (cons callee) (str/join " ")) ")")
      (str "(." (second callee) " " (first callee) " " (str/join " " args)
           ")"))))

(defmethod parse-frag "IfStatement" [{:keys [test consequent alternate]} state]
  (if alternate
    (str "(if "
         (parse-frag test (assoc state :single? true))
         " " (parse-frag consequent (assoc state :single? true))
         " " (parse-frag alternate (assoc state :single? true))
         ")")
    (str "(when "
         (parse-frag test (assoc state :single? true))
         " " (parse-frag consequent state)
         ")")))

(defn- random-identifier [] (gensym "-temp-"))
(defmethod parse-frag "FunctionDeclaration" [{:keys [id params body]} state]
  (let [params (map #(parse-frag % state) params)
        body (parse-frag body (assoc state :single? false))
        params-detailed (for [param params]
                          (if (vector? param)
                            {:fun (random-identifier) :extracts-to param}
                            {:fun param}))
        let-params (filter :extracts-to params-detailed)
        norm-body (if (seq let-params)
                    (let [lets (for [{:keys [fun extracts-to]} let-params
                                     [k v] extracts-to]
                                 (str k " (.-" v " " fun ")"))]
                      (str "(let [" (str/join " " lets) "] " body ")"))
                    body)]
    (str "(defn " (parse-frag id state)
         " [" (->> params-detailed (map :fun) (str/join " ")) "] " norm-body ")")))

(defn- parse-fun [{:keys [id params body]} state]
  (let [params (->> params (map #(parse-frag % state)) (str/join " "))
        body (parse-frag body (assoc state :single? false))]
    (str "(fn"
         (when-let [name (some-> id (parse-frag state))]
           (str " " name))
         " [" params "] " body ")")))

(defmethod parse-frag "FunctionExpression" [step state] (parse-fun step state))
(defmethod parse-frag "ArrowFunctionExpression" [step state] (parse-fun step state))

(defmethod parse-frag "ReturnStatement" [{:keys [argument]} state]
  (parse-frag argument state))

(defmethod parse-frag "AssignmentExpression" [{:keys [operator left right]} state]
  (let [vars (parse-frag left (assoc state :single? true))]
    (str "(def "
         vars
         " " (parse-frag right (assoc state :single? true))
         ")")))

(defn- make-destr-def [[k v] val]
  (str "(def " k " (.-" v " " val "))"))

(defmethod parse-frag "VariableDeclaration" [{:keys [declarations]} state]
  (let [declarations (mapv #(parse-frag % state) declarations)]
    (when (:root? state)
      (let [defs (for [[k v] declarations]
                   (if (vector? k)
                     (if (-> k count (= 1))
                       (make-destr-def (first k) v)
                       (let [sym (random-identifier)
                             inner (map #(make-destr-def % sym) k)]
                         (str "(let [" sym " " v "] " (str/join " " inner) ")")))
                     (str "(def " k " " v ")")))]
        (str/join " " defs)))))

(defmethod parse-frag "VariableDeclarator" [{:keys [id init]} state]
  (let [vars (:locals state)
        init (if init
               (parse-frag init (assoc state :single? true))
               "nil")
        body [(parse-frag id (assoc state :single? true)) init]]
    (if vars
      (swap! vars conj body)
      body)))

(defmethod parse-frag "ObjectExpression" [{:keys [properties]} state]
  (let [kvs (->> properties
                 (map #(parse-frag % (assoc state :single? true)))
                 (map (fn [[k v]] (str ":" k " " v))))]
    (str "#js {" (str/join " " kvs) "}")))

(defmethod parse-frag "ArrayExpression" [{:keys [elements]} state]
  (let [vals (map #(parse-frag % (assoc state :single? true)) elements)]
    (str "#js [" (str/join " " vals) "]")))

(defmethod parse-frag "Property" [{:keys [key value]} state]
  [(parse-frag key (assoc state :single? true))
   (parse-frag value (assoc state :single? true))])

(defmethod parse-frag "MemberExpression" [{:keys [object property]} state]
  (if (:calling? state)
    [(parse-frag object state) (parse-frag property state)]
    (str "(.-" (parse-frag property state)
         " " (parse-frag object state) ")")))

(defmethod parse-frag "ObjectPattern" [{:keys [properties]} state]
  (mapv #(parse-frag % (assoc state :single? true))
        properties))

(defmethod parse-frag :default [dbg state]
  (tap> dbg)
  (def t (:type dbg))
  (throw (ex-info (str "Not implemented: " (:type dbg))
                  {:element (:type dbg)})))

#_
(parse-str "a={a: 10, b: 20}")

#_(from-js "a.b")
#_(from-js "a={a: 10, b: 20}")

(defn- from-js [code]
  (-> code
      (parse #js {:ecmaVersion 2020})
      js/JSON.stringify
      js/JSON.parse
      (js->clj :keywordize-keys true)))

(defn parse-str
  ([code]
   (-> code
       from-js
       (parse-frag {})))
  ([code format-opts]
   (-> code
       parse-str
       (zprint (clj->js format-opts)))))
