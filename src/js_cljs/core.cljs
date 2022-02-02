(ns js-cljs.core
  (:require ["acorn" :refer [parse]]
            [clojure.string :as str]))

(defmulti parse-frag (fn [step _] (:type step)))

(defn- block [step state]
  (let [ops (map #(parse-frag % {}) (:body step))
        body (str/join " " (map #(parse-frag % {}) (:body step)))]
    (if (-> state :single? not (or (-> ops count (= 1))))
      body
      (str "(do " body ")"))))

(defmethod parse-frag "Program" [step state] (block step state))
(defmethod parse-frag "BlockStatement" [step state] (block step state))

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
         " " (parse-frag left {:single? true})
         " " (parse-frag right {:single? true})
         ")")))

(defmethod parse-frag "UnaryExpression" [{:keys [operator argument]} state]
  (let [operator (get-operator operator)]
    (str "(" operator " " (parse-frag argument {:single? true}) ")")))

(defmethod parse-frag "BinaryExpression" [step state] (binary-exp step state))
(defmethod parse-frag "LogicalExpression" [step state] (binary-exp step state))

(defmethod parse-frag "Literal" [{:keys [value]} _] value)
(defmethod parse-frag "Identifier" [{:keys [name]} _] name)
(defmethod parse-frag "CallExpression" [{:keys [callee arguments]} state]
  (let [body (cons (parse-frag callee (assoc state :single? true))
                   (map #(parse-frag % (assoc state :single? true)) arguments))]
    (str "(" (str/join " " body) ")")))

(defmethod parse-frag "IfStatement" [{:keys [test consequent alternate]} _]
  (if alternate
    (str "(if "
         (parse-frag test {:single? true})
         " " (parse-frag consequent {:single? true})
         " " (parse-frag alternate {:single? true})
         ")")
    (str "(when "
         (parse-frag test {:single? true})
         " " (parse-frag consequent {})
         ")")))

(defmethod parse-frag "FunctionDeclaration" [{:keys [id params body]} state]
  (let [params (->> params (map #(parse-frag % state)) (str/join " "))
        body (parse-frag body (assoc state :single? false))]
    (str "(defn " (parse-frag id state)
         " [" params "] " body ")")))

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

#_(from-js "(function(a, b) { return a + b})(1, 2)")

(defn- from-js [code]
  (-> code
      (parse #js {:ecmaVersion 2020})
      js/JSON.stringify
      js/JSON.parse
      (js->clj :keywordize-keys true)))

(defn parse-str [code]
  (-> code
      from-js
      (parse-frag {})))

(defn main []
  (prn :HELLO))
