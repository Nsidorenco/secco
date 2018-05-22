(ns secco.interpreter
  (:require [instaparse.core :as insta]
            [clojure.core.match :refer [match]]
            [secco.cfg :as cfg]))

(defn- op-exp
  [exp1 oper exp2]
  (if (= "!=" (str oper))
    (not= exp1 exp2)
    ((resolve oper) exp1 exp2)))

(defn- expression
  [exp env]
  (letfn [(arithmetic [exp oper]
            (let [[exp1 exp2] (rest exp)
                  exp1 (first (expression exp1 env))
                  exp2 (first (expression exp2 env))]
              [(oper exp1 exp2) env]))]
    (match [(first exp)]
      [:INT] [(read-string (second exp)) env]
      [:OPER] [(read-string (second exp)) env]
      [:add] (arithmetic exp +)
      [:sub] (arithmetic exp -)
      [:mul] (arithmetic exp *)
      [:UserInput] (do (println "Enter input: ")
                       (flush)
                       [(read-string (read-line)) env])
      [:Error] (throw (Exception. "Error state reached"))
      [:VarExp] (match [(first (second exp))]
                       [:SimpleVar] (let [value (get env (second (second exp)))]
                                      (assert (not= value nil) "Variable not declared")
                                      [value env])
                       [:ArrayVar] (let [arr (get env (second (second exp)))
                                         array-index (read-string (second (last (second exp))))]
                                     [(nth arr array-index) env]))
      [:OpExp] (let [[exp1 oper exp2] (rest exp)
                     exp1 (first (expression exp1 env))
                     exp2 (first (expression exp2 env))
                     oper (first (expression oper env))
                     res (op-exp exp1 oper exp2)]
                 [res env])
      [:ParenExp] (recur (second exp) env)
      [:Array] (let [arraysize (read-string (second (second exp)))]
                 [(vec (repeat arraysize nil)) (vec (repeat arraysize nil))])
      [:AssignExp] (let [[varexp body] (rest exp)
                         varname (second (second varexp))
                         body (first (expression body env))]
                     (if (= (first (second varexp)) :ArrayVar)
                       (let [arr (get env varname)
                             array-index (read-string (second (last (second varexp))))]
                         [body (conj env {varname (assoc arr array-index body)})])
                       [body (conj env {varname body})]))
      [_] [0 env])))

(defn interpret
  ([node] (interpret node {}))
  ([node env]
   (if (cfg/node? node)
     (let [[res env] (expression (.exp node) env)
           env (with-meta env {:res res})]
       (if (false? res)
         (recur (cfg/get-f node) env)
         (recur (cfg/get-t node) env)))
     (-> env meta :res))))

;(println (interpret (cfg/build "x := array(4); x[1] := 2; if x[1] = 2 then error() else 2")))
;(println (interpret (cfg/build "x := 3; x")))
; (interpret (cfg/build "(x := input(); if x > 2 then 2 else 4)"))
; (interpret (cfg/build (slurp (clojure.java.io/resource "test-programs/gcd.sec"))))
;(interpret (cfg/build "x := input(); while x < 10 do x := x+1 end; x"))