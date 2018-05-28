(ns secco.symbolic
  (:require [secco.cfg :as cfg]
            [z3.solver :as z3]
            [secco.util :as util]
            [clojure.core.match :refer [match]]))

; define mutable variables
(defonce ^:dynamic *venv* {})
(defonce sym (atom (vec util/literals)))
(defonce nodes-visited (atom 0))
(defonce active-threads (atom 0))

; define an channel responsible for passing error states
(defonce error-queue (java.util.concurrent.LinkedBlockingQueue.))

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
      [:VarExp] (match [(first (second exp))]
                       [:SimpleVar] (let [value (get *venv* (second (second exp)))]
                                      (assert (not= value nil) "Variable not declared")
                                      value)
                       [:ArrayVar] (let [arr (get *venv* (second (second exp)))
                                         array-index (read-string (second (last (second exp))))]
                                     (nth arr array-index)))
      [:Array] (let [uid (gensym)
                     arraysize (read-string (second (second exp)))
                     array (reduce (fn [acc e] (conj acc (str uid e))) [] (range arraysize))]
                 (set! *venv* (conj *venv* (reduce (fn [acc e] (conj acc {e 0})) {} array)))
                 uid)
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
                         varname (second (last varexp))
                         body (sym-exp bodyexp)]
                     (if (= (first (last varexp)) :ArrayVar)
                       (let [idx (read-string (second (last (second (second exp)))))
                             uid (get *venv* varname)]
                         (set! *venv* (conj *venv* {(str uid idx) body})))
                       (do (set! *venv* (conj *venv* {varname body}))
                           (when (= (first bodyexp) :UserInput)
                             (z3/const body Int)))))
      [_] "")))

(defn model
  ([node] (model node []))
  ([node pc]
   (when (and (z3/check-sat pc) (instance? secco.cfg.CFGNode node))
     (swap! nodes-visited inc)
     (let [last_cond (sym-exp (.exp node))
           _ (println *venv*)]
       (if (instance? clojure.lang.PersistentList last_cond)
         (let [t_pc (conj pc last_cond)
               f_pc (conj pc (z3/not last_cond))]
           (future (do
                     (swap! active-threads inc)
                     (model (cfg/get-f node) f_pc)
                     (swap! active-threads dec)))
           (recur (cfg/get-t node) t_pc))
         (if (isa? ::Error last_cond)
           (.put error-queue
                 (str "Reached error state on line: " (.start node)
                      "," (.end node)
                      " for expression: " (.exp node)
                      "\n with solution: " (z3/solve pc)
                      " for: " *venv*))
           (model (cfg/get-t node) pc)))))))

(defn execute
  [cfg]
  (reset! nodes-visited 0)
  (reset! active-threads 0)
  (binding [*venv* {}]
    (model cfg)
    (while (or (> @active-threads 0) (not (.isEmpty error-queue)))
      (println (.take error-queue)))
    (println "Coverage was:" (* (/ @nodes-visited @cfg/node-count) 100) "%"))
  (shutdown-agents))

;(execute (cfg/build "(x:=input(); a:=array(4); a[2]:=3; a[3]:=10)"))
;(execute (cfg/build "a:=array(4); a[2]:=3; if a[2] = 1 then error() else error()"))
; (model (cfg/build "(a := 1 + 1; if a > 1 then 2 else 3)"))
; (z3/check-sat (conj [] (z3/const x Int)))
; (execute (cfg/build "(x:= input();if x < 0 then error() else error())"))
; (model (cfg/build "if 1 < 2 then 3 else 41"))
;(z3/check-sat (model (cfg/build "(x:=0;y:=8;while x<y do x:=x+1;y:=5)")))
; (model (cfg/build "(x:=input();y:=input(); if x<y then x:=x+y else y:=x)"))
; (model (cfg/build "(x:=input(); if x > 10 then error() else error())"))
; (model (cfg/build (slurp (clojure.java.io/resource "test-programs/gcd.sec"))))
