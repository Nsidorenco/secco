(ns secco.interpreter
  [:require [instaparse.core :as insta]])

(def venv
  {})

(defn interpret-expression
  [exp]
  (insta/transform 
    {:INT read-string,
     :OPER (comp read-string str),
     :OpExp (fn [exp1 oper exp2]
              (println oper)
              (if (= oper "PLUS")
                (+ exp1 exp2)
                (println oper "PLUS")))}
    exp))

(defn interpret [node]
  (let [guard (.exp node)]
    (if guard (interpret (get-t node) (interpret (get-f node))))))

(interpret-expression [:OpExp [:Int "4"] [:OPER "PLUS"] [:INT "4"]])
(interpret-expression [:OPER "+"])
