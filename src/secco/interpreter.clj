(ns secco.interpreter
  (:require [instaparse.core :as insta]
            [clojure.core.match :refer [match]]
            [secco.cfg :as cfg]))

; define mutable variables
(def venv (atom {}))
(def res (atom 0))

(defmacro set-res
  ([value]
   `(reset! res ~value))
  ([exp1 oper exp2]
   `(set-res ((resolve ~oper) ~exp1 ~exp2))))

(defn interpret-expression
  [exp]
  (let [arithmetic (fn [exp oper]
                     (let [[_ exp1 exp2] exp]
                       (interpret-expression exp1)
                       (let [exp1 @res]
                         (interpret-expression exp2)
                         (let [exp2 @res]
                           (reset! res (oper exp1 exp2))))))]
    (match [(first exp)]
      [:INT] (set-res (read-string (second exp)))
      [:OPER] (read-string (second exp))
      [:add] (arithmetic exp +)
      [:sub] (arithmetic exp -)
      [:mul] (arithmetic exp *)
      [:UserInput] (do (println "Enter input: ")
                       (flush)
                       (set-res (read-string (read-line))))
      [:Error] (throw (Exception. "Error state reached"))
      [:VarExp] (let [varname (get @venv (second exp))]
                  (set-res varname)
                  (assert (not= @res nil) "Variable not declared"))
      [:OpExp] (let [[_ exp1 oper exp2] exp]
                 (interpret-expression exp1)
                 (let [exp1 @res]
                   (interpret-expression exp2)
                   (let [exp2 @res
                         oper (interpret-expression oper)]
                     (if (= "!=" (str oper))
                       (set-res (not (= exp1 exp2)))
                       (set-res exp1 oper exp2)))))
      [:ParenExp] (interpret-expression (second exp))
      [:AssignExp] (let [[_ varexp body] exp
                         varname (second varexp)]
                     (interpret-expression body)
                     (assert (not= @res nil) (println "Error! Variable not declared"))
                     (swap! venv conj {varname @res}))
      [_] @res)))

(defn interpret [node]
  (if (instance? secco.cfg.CFGNode node)
    (let [guard (.exp node)]
      (interpret-expression guard)
      (if @res
        (recur (cfg/get-t node))
        (recur (cfg/get-f node))))
    (println "res = " @res " venv = " @venv)))
