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
                       [:SimpleVar] (let [value (get env (read-string (second (second exp))))]
                                      (assert (not= value nil) "Variable not declared")
                                      [value env])
                       [:ArrayVar] (let [arr (get env (read-string (second (second exp))))
                                         array-index (read-string (second (last (second exp))))
                                         val (get env (str arr array-index))]
                                     (assert (not= val nil) "Variable not declared")
                                     [val env]))
      [:OpExp] (let [[exp1 oper exp2] (rest exp)
                     exp1 (first (expression exp1 env))
                     exp2 (first (expression exp2 env))
                     oper (first (expression oper env))
                     res (op-exp exp1 oper exp2)]
                 [res env])
      [:ParenExp] (recur (second exp) env)
      [:Array] (let [uid (gensym)
                     arraysize (read-string (second (second exp)))
                     array (reduce (fn [acc e] (conj acc (str uid e))) [] (range arraysize))]
                  [uid (conj env (reduce (fn [acc e] (conj acc {e 0})) {} array))])
      [:AssignExp] (let [[varexp body] (rest exp)
                         varname (second (second varexp))
                         [body env'] (expression body env)]
                        (if (= :ArrayVar (first (second (second exp))))
                          (let [idx (read-string (second (last (second (second exp)))))
                                uid (get env (read-string varname))]
                            [body (conj env env' {(str uid idx) body})])
                          [body (conj env env' {(read-string varname) body})]))

      [_] [0 env])))

(defn interpret
  ([node] (interpret node {}))
  ([node env]
   (if (cfg/node? node)
     (let [[res env] (expression (.exp node) env)
           env (with-meta env {:res res})
           _ (println env)]
       (if (false? res)
         (recur (cfg/get-f node) env)
         (recur (cfg/get-t node) env)))
     (-> env meta :res))))

;(println (interpret (cfg/build "x := array(4); x[1] := 2; if x[1] = 2 then error() else 2")))
;(println (interpret (cfg/build "(x:=array(4); x[3]:=input())")))
;(interpret (cfg/build "(x := input(); if x > 2 then 2 else 4)"))
; (interpret (cfg/build (slurp (clojure.java.io/resource "test-programs/gcd.sec"))))
;(interpret (cfg/build "x := input(); while x < 10 do x := x+1 end; x"))