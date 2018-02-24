(ns secco.symbolic
  [:require [instaparse.core :as insta]
            [secco.cfg :as cfg]
            [z3.solver :as z3]
            [secco.util :as util]
            [clojure.core.match :refer [match]]])

; define mutable variables
(def venv (atom {}))
(def sym (atom (into [] util/literals)))

(defn sym-exp
  [exp]
  (let [getsym (util/pop-head! sym)]
  (match [(first exp)]
         [:INT] (read-string (second exp)) 
         [:OPER] (read-string (second exp))
         [:VarExp] (let [varname (get @venv (second exp))]
                     (assert (not= varname nil) "Variable not declared")
                     varname)  
         [:OpExp] (let [[_ exp1 oper exp2] exp
                         e1 (sym-exp exp1)
                         op (sym-exp oper)
                         e2 (sym-exp exp2)]
                    (z3/assert e1 op e2))
         [:AssignExp] (let [[_ varexp bodyexp] exp
                            varname (second varexp)
                            body (sym-exp bodyexp)]
                        (swap! venv conj {varname getsym})
                        (z3/const (get @venv varname) Int))
         [_] "")))

(defn model 
  ([node] (model node []))
  ([node pc] 
   (if (instance? secco.cfg.CFGNode node)
     (let [last_cond (sym-exp (.exp node))]
       (if (instance? clojure.lang.PersistentList last_cond)
         (let [t_pc (conj pc last_cond)
               f_pc (conj pc (z3/not last_cond))]
           (when (z3/check-sat t_pc) 
             (model (cfg/get-t node) t_pc))
           (when (z3/check-sat f_pc) 
             (model (cfg/get-f node) f_pc))
           (when-not (or (z3/check-sat t_pc) (z3/check-sat f_pc))
             (println "pc: " pc)
             pc))
         (do 
           (model (cfg/get-t node) pc))))
     (println "pc: "pc))))

;(z3/check-sat (conj [] (z3/const x Int)))
; (reset! venv {})
; (println @venv)
; (get @venv "x")
; (model (cfg/build "(x:=0;if x < 0 then 0 else x)")) 
; (model (cfg/build "if 1 < 2 then 3 else 41"))

;z3/check-sat (model (cfg/build "(x:=0;y:=8;while x<y do x:=x+1;y:=5)")))

