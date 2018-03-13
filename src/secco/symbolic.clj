(ns secco.symbolic
  (:require [secco.cfg :as cfg]
            [z3.solver :as z3]
            [secco.util :as util]
            [clojure.core.match :refer [match]]))

; define mutable variables
(def venv (atom {}))
(def sym (atom (vec util/literals)))

(defmacro arithmetic
  [sym exp1 exp2]
  `(list (symbol '~sym) ~exp1 ~exp2))

(defn sym-exp
  [exp]
  (let [getsym (util/pop-head! sym)]
    (match [(first exp)]
      [:INT] (read-string (second exp))
      [:OPER] (read-string (second exp))
      [:UserInput] getsym
      [:Error] ::Error
      [:VarExp] (let [varname (get @venv (second exp))]
                  (assert (not= varname nil) "Variable not declared")
                  varname)
      [:OpExp] (let [[_ exp1 oper exp2] exp
                     e1 (sym-exp exp1)
                     op (sym-exp oper)
                     e2 (sym-exp exp2)]
                 (z3/assert e1 op e2))
      [:add] (let [[_ exp1 exp2] exp]
               (arithmetic + (sym-exp exp1) (sym-exp exp2)))
      [:sub] (let [[_ exp1 exp2] exp]
               (arithmetic - (sym-exp exp1) (sym-exp exp2)))
      [:mul] (let [[_ exp1 exp2] exp]
               (arithmetic * (sym-exp exp1) (sym-exp exp2)))
      [:AssignExp] (let [[_ varexp bodyexp] exp
                         varname (second varexp)
                         body (sym-exp bodyexp)]
                     (swap! venv conj {varname body})
                     (when (= (first bodyexp) :UserInput)
                       (z3/const body Int)))
      [_] "")))

(defn model
  ([node] (model node []))
  ([node pc]
   (when (z3/check-sat pc)
     (if (instance? secco.cfg.CFGNode node)
       (let [last_cond (sym-exp (.exp node))]
         (if (instance? clojure.lang.PersistentList last_cond)
           (let [t_pc (conj pc last_cond)
                 f_pc (conj pc (z3/not last_cond))]
             (model (cfg/get-f node) f_pc)
             (model (cfg/get-t node) t_pc))
           (if (isa? ::Error last_cond)
             (println "Reached error state on line: " (.start node)
                      "," (.end node)
                      "for expression: " (.exp node)
                      "with solution: " (z3/solve pc))
             (model (cfg/get-t node) pc))))
       (println "pc: " pc)))))

; (model (cfg/build "(a := 1 + 1; if a > 1 then 2 else 3)"))
; (z3/check-sat (conj [] (z3/const x Int)))
; (reset! venv {})
; (println @venv)
; (get @venv "x")
; (model (cfg/build "(x:=0;if x < 0 then 0 else x)")) 
; (model (cfg/build "if 1 < 2 then 3 else 41"))
;(z3/check-sat (model (cfg/build "(x:=0;y:=8;while x<y do x:=x+1;y:=5)")))
; (model (cfg/build "(x:=input();y:=input(); if x<y then x:=x+y else y:=x)"))
; (model (cfg/build "(x:=input(); if x > 10 then error() else 1)"))
; (model (cfg/build (slurp (clojure.java.io/resource "test-programs/gcd.sec"))))
