(ns secco.symbolic
  [:require [instaparse.core :as insta]
            [secco.cfg :as cfg]
            [secco.z3 :as z3]
            [clojure.core.match :refer [match]]])

; define mutable variables
(def venv (atom {}))
(def res (atom []))


(defmacro set-res
  ([value]
  `(reset! res ~value))
  ([exp1 oper exp2]
  `(set-res (z3/assert exp1 oper exp2))))


(defn sym-exp
  [exp]
  (match [(first exp)]
         [:INT] (read-string (second exp)) 
         [:OPER] (read-string (second exp))
         [:VarExp] (let [varname (get @venv (second exp))]
                     (assert (not= @res nil) "Variable not declared"))  
         [:OpExp] (let [[_ exp1 oper exp2] exp]
                    (sym-exp exp1)
                    (let [exp1 @res]
                      (sym-exp exp2)
                      (let [exp2 @res oper (sym-exp oper)]
                        (set-res exp1 oper exp2))))
         [:AssignExp] (let [[_ varexp bodyexp] exp
                            varname (sym-exp (second varexp))
                            body (sym-exp bodyexp)]
                        (set-res (z3/const varname Int)))
         [_] @res))

(defn model 
  ([node] (model node []))
  ([node build]
   (if (instance? secco.cfg.CFGNode node)
     (let [
           tr (sym-exp (.exp (cfg/get-t node)))
           fa (sym-exp (.exp (cfg/get-f node)))
           ]
       (model (cfg/get-t node) (conj build tr))
       (model (cfg/get-f node) (conj build fa))))
   build))

(z3/check-sat (model (cfg/build "(x:=0;y:=8;while x<y do x:=x+1;y:=5)")))

(def whilemodel (conj [] 
                      (z3/const x Int)
                      (z3/const y Int)
                      (z3/assert x < y)))

(z3/check-sat whilemodel)
