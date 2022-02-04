(ns js-cljs.core
  (:require ["acorn" :refer [parse]]
            [zprint.core :as zprint]
            [clojure.string :as str]))

(defmulti parse-frag (fn [step _] (:type step)))

(defn- block [step state sep]
  (let [ops (map #(parse-frag % state) (:body step))
        body (->> (:body step)
                  (map #(parse-frag % state))
                  (remove nil?)
                  (str/join sep))
        locals (:locals state)]
    (cond
      (and locals (seq @locals)) (str "(let [" (str/join " " (mapcat identity @locals)) "] " body ")")
      (-> state :single? not (or (-> ops count (= 1)))) body
      :else (str "(do " body ")"))))

(defmethod parse-frag "Program" [step state]
  (block step (assoc state :root? true) "\n"))

(defmethod parse-frag "BlockStatement" [step state]
  (block step (assoc state :root? false :locals (atom [])) " "))

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

(defn- call-expr [{:keys [callee arguments]} state]
  (let [callee (parse-frag callee (assoc state :single? true :special-js? true))
        args (mapv #(parse-frag % (assoc state :single? true)) arguments)]
    (if (string? callee)
      (if (-> args peek vector?)
        (str "(apply " (->> (update args (-> args count dec) peek) (cons callee) (str/join " ")) ")")
        (str "(" (->> args (cons callee) (str/join " ")) ")"))
      (str "(." (second callee) " " (first callee) " " (str/join " " args)
           ")"))))
(defmethod parse-frag "CallExpression" [prop state] (call-expr prop state))
(defmethod parse-frag "NewExpression" [props state]
  (call-expr (update-in props [:callee :name] str ".") state))

(defn- if-then-else [{:keys [test consequent alternate]} state]
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

(defmethod parse-frag "IfStatement" [element state] (if-then-else element state))
(defmethod parse-frag "ConditionalExpression" [element state] (if-then-else element state))

(defn- random-identifier [] (gensym "-temp-"))
(defn- to-obj-params [fun param]
  (map (fn [[k v]] (str k " (.-" v " " fun ")")) param))

(defn- to-default-param [[fun default]]
  [fun (str "(if (undefined? " fun ") " default " " fun ")")])

(defmethod parse-frag "FunctionDeclaration" [{:keys [id params body]} state]
  (let [params (map #(parse-frag % state) params)
        body (parse-frag body (assoc state :single? false))
        params-detailed (for [param params]
                          (if (vector? param)
                            (if (-> param first vector?)
                              (let [id (random-identifier)]
                                {:fun id :extracts-to (to-obj-params id param)})
                              {:fun (first param) :extracts-to (to-default-param param)})
                            {:fun param}))
        let-params (->> params-detailed (mapcat :extracts-to) (filter identity))
        norm-body (if (seq let-params)
                    (str "(let [" (str/join " " let-params) "] " body ")")
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
  (let [vars (parse-frag left (assoc state :single? true :special-js? true))
        val (parse-frag right (assoc state :single? true))]
    (if (string? vars)
      (str "(def " vars " " val ")")
      (str "(aset " (first vars) " " (-> vars second pr-str) " " val ")"))))

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
                     (if (and (string? v) (str/starts-with? v "(fn "))
                       (str "(defn " k " " (subs v 4))
                       (str "(def " k " " v ")"))))]
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
  (if (:special-js? state)
    [(parse-frag object state) (parse-frag property state)]
    (str "(.-" (parse-frag property state)
         " " (parse-frag object state) ")")))

(defmethod parse-frag "ObjectPattern" [{:keys [properties]} state]
  (mapv #(parse-frag % (assoc state :single? true))
        properties))

(defmethod parse-frag "AssignmentPattern" [{:keys [left right]} state]
  [(parse-frag left (assoc state :single? true))
   (parse-frag right (assoc state :single? true))])

(defmethod parse-frag "SpreadElement" [{:keys [argument]} state]
  [(parse-frag argument)])

(defn- gen-properties [class [property {:keys [get set]}]]
  (str "(.defineProperty js/Object (.-prototype " class
       ") " (pr-str property) " #js {"
       (when get
         (str ":get (fn [] " (str/replace-first get #".*this\]" "(this-as this")))
       (when set
         (let [[_ params] (re-find #"\[this (.*)\]" set)]
          (str ":set (fn [" params "] "
               (str/replace-first set #".*this.*\]" "(this-as this"))))
       ")})"))

(defn- class-declaration [{:keys [id, superClass, body]} state]
  (swap! (:cljs-requires state) conj '[shadow.cljs.modern :as modern])
  (let [class-name (parse-frag id state)
        {:keys [constructor methods properties]} (parse-frag body state)
        super (some-> superClass (parse-frag state))
        defclass (str "(modern/defclass " class-name
                      (when super (str " (extends " super ")"))
                      " "
                      (if constructor constructor "(constructor [this])")
                      (when (seq methods)
                        (->> methods (cons " Object") (str/join " ")))
                      ")")]
    (cond-> defclass
      properties (str (->> properties
                           (map #(gen-properties class-name %))
                           (cons "")
                           (str/join " "))))))


(defmethod parse-frag "ClassDeclaration" [props state] (class-declaration props state))
(defmethod parse-frag "ClassExpression" [props state] (class-declaration props state))

(defmethod parse-frag "ClassBody" [{:keys [body]} state]
  (reduce (fn [acc b]
            (case (:kind b)
              "constructor" (assoc acc :constructor (parse-frag b state))
              "get" (assoc-in acc [:properties (-> b :key :name) :get] (parse-frag b state))
              "set" (assoc-in acc [:properties (-> b :key :name) :set] (parse-frag b state))
              (update acc :methods conj (parse-frag b state))))
          {:methods []}
          body))

(defmethod parse-frag "MethodDefinition" [{:keys [key value]} state]
  (let [{:keys [params body]} value]
    (str "(" (parse-frag key state)
         " [" (->> params
                   (map #(parse-frag % state))
                   (cons "this")
                   (str/join " "))
         "]" (some->> (parse-frag body state) not-empty (str " "))
         ")")))

(defmethod parse-frag :default [dbg state]
  (tap> dbg)
  (def t (:type dbg))
  (throw (ex-info (str "Not implemented: " (:type dbg))
                  {:element (:type dbg)})))

#_
(parse-str "const a = class B {}")

#_(from-js "const a = class B {}")
#_(from-js "class B { get a() { return 10 } }")

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
       (parse-frag {:cljs-requires (atom [])})))
  ([code format-opts]
   (-> code
       parse-str
       (zprint/zprint-file-str "file: example.cljs" format-opts))))
