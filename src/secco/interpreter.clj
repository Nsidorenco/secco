(ns secco.interpreter
  [:require [instaparse.core :as insta] ]
  [:use  [secco.cfg]])

(def venv (atom {}))
(swap! venv conj {:a 3})
(println (get @venv :a))

(def res (atom 0))

(defn interpret-expression
  [exp]
  (insta/transform 
    {:INT (fn [x] (reset! res (read-string x)))
     :OPER read-string
     :VarExp (fn [x] (reset! res (read-string x)))
     :OpExp (fn [exp1 oper exp2]
              (cond (= (str oper) "+") (reset! res (+ exp1 exp2))
                    (= (str oper) "<") (reset! res (< exp1 exp2))
                    :else (println "oper not supported")))
     :AssignExp (fn [varexp body]
                  (interpret-expression varexp)
                  (let [varname @res]
                    (interpret-expression body)
                    (swap! venv conj (varname @res))))}
    exp))

(defn interpret [node]
  (if (instance? secco.cfg.CFGNode node)
    (let [guard (.exp node)]
      (interpret-expression guard)
      (if @res
        (interpret (get-t node))
        (interpret (get-f node))))
    (println @res)))

(interpret-expression [:OpExp [:INT "4"] [:OPER "+"] [:INT "4"]])
(interpret-expression [:OPER "+"])

(interpret (get-t(build "x := 4")))
(interpret (build "if 3 < 2 then 4 else 5"))
(interpret-expression (.exp (get-t(build "4+4"))))
(interpret (->Node "oper" (.exp (get-t(build "4+4"))) nil nil))
(interpret (build "4+4"))

(instance? secco.cfg.CFGNode "")
