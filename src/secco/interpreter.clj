(ns secco.interpreter
  [:require [instaparse.core :as insta]
            [clojure.core.match :refer [match]]]
  [:use  [secco.cfg]])

; define mutable variables
(def venv (atom {}))
(def res (atom 0))

(defn interpret-expression
  [exp]
  (match [(first exp)]
         [:INT] (reset! res (read-string (second exp)))
         [:OPER] (read-string (second exp))
         [:VarExp] (let [varname (get @venv (second exp))]
                    (reset! res varname)
                    (assert (not= @res nil) "Variable not declared"))
         [:OpExp] (let [[_ exp1 oper exp2] exp]
                    (interpret-expression exp1)
                    (let [exp1 @res]
                      (interpret-expression exp2)
                      (let [exp2 @res oper (interpret-expression oper)]
                        (cond (= (str oper) "+") (reset! res (+ exp1 exp2))
                              (= (str oper) "-") (reset! res (- exp1 exp2))
                              (= (str oper) "<") (reset! res (< exp1 exp2))
                              (= (str oper) ">") (reset! res (> exp1 exp2))
                              (= (str oper) "<=") (reset! res (<= exp1 exp2))
                              (= (str oper) ">=") (reset! res (>= exp1 exp2))
                              (= (str oper) "=") (reset! res (= exp1 exp2))
                              (= (str oper) "!=") (reset! res (not= exp1 exp2))
                              :else (println "oper not supported")))))
         [:AssignExp] (let [[_ varexp body] exp]
                        (let [varname (second varexp)]
                          (interpret-expression body)
                          (swap! venv conj {varname @res})))
         [_] @res))

(defn interpret [node]
  (if (instance? secco.cfg.CFGNode node)
    (let [guard (.exp node)]
      (interpret-expression guard)
      (if @res
        (interpret (get-t node))
        (interpret (get-f node))))
    (println "res = " @res " venv = " @venv)))

; (println "venv = " @venv)
; (interpret-expression [:OpExp [:INT "4"] [:OPER "+"] [:INT "4"]])
; (interpret-expression [:OPER "+"])

; (interpret (build "(x := 4; x := 7; x := 1021212131)"))
; (interpret (build "if 3 < 2 then 4 else 5"))
; (interpret-expression (.exp (get-t(build "4+4"))))
; (interpret (->Node "oper" (.exp (get-t(build "4+4"))) nil nil))
; (interpret (build "4+4"))

(get-t(get-t(get-t(get-t (build "(x:=0;z:=0;while x<6 do (x := x+1; while z<6 do z := z+1))")))))
(get-f(get-t(get-t(get-t (get-t (get-t (build "(x:=0;z:=0;while x<6 do (x := x+1; while z<6 do z := z+1))")))))))