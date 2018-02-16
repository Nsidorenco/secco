(ns secco.interpreter
  [:require [instaparse.core :as insta] ]
  [:use  [secco.cfg]])

(def venv
  {})

(defn interpret-expression
  [exp]
  (insta/transform 
    {:INT read-string,
     :OPER read-string,
     :OpExp (fn [exp1 oper exp2]
              (if (= (str oper) "+")
                (+ exp1 exp2)
                (println oper "+")))}
    exp))

(defn interpret [node]
  (let [guard (.exp node)]
    (if guard 
      (interpret (get-t node))
      (interpret (get-f node)))))

(interpret-expression [:OpExp [:INT "4"] [:OPER "+"] [:INT "4"]])
(interpret-expression [:OPER "+"])
