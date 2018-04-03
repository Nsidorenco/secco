(ns secco.concolic
  (:require [z3.solver :as z3]
            [secco.cfg :as cfg]
            [secco.util :as util]
            [clojure.core.match :refer [match]]))

(def inputs (atom {}))

(defn- op-exp
  [exp1 oper exp2]
  (if (= "!=" (str oper))
    (not (= exp1 exp2))
    (eval ((resolve oper) exp1 exp2))))

(defn- concrete
  [exp env]
  (letfn [(arithmetic [exp oper]
            (let [[exp1 exp2] (rest exp)
                  exp1 (first (concrete exp1 env))
                  exp2 (first (concrete exp2 env))]
              [(oper exp1 exp2) env]))]
    (let [res (match [(first exp)]
                [:INT] [(read-string (second exp)) env]
                [:OPER] [(read-string (second exp)) env]
                [:add] (arithmetic exp +)
                [:sub] (arithmetic exp -)
                [:mul] (arithmetic exp *)
                [:Error] [::Error env]
                [:UserInput] [(rand-int 1000) env]
                [:VarExp] (let [value (get env (read-string (second exp)))]
                            (assert (not= value nil)
                                    (str (second exp)
                                         " Variable not declared (concrete)"))
                            [value env])
                [:OpExp] (let [[exp1 oper exp2] (rest exp)
                               exp1 (first (concrete exp1 env))
                               exp2 (first (concrete exp2 env))
                               oper (first (concrete oper env))
                               res (op-exp exp1 oper exp2)]
                           (if res
                             (with-meta [[] env] {:path true})
                             (with-meta [[] env] {:path false})))
                [:ParenExp] (concrete (second exp) env)
                [:AssignExp] (let [[varexp bodyexp] (rest exp)
                                   varname (read-string (second varexp))
                                   body (first (concrete bodyexp env))]
                               [body (conj env {varname body})])
                [_] [[] env])]
      (if (nil? (:path (meta res)))
        (with-meta res {:path true})
        res))))

(defmacro arithmetic
  [sym exp1 exp2]
  `(list (symbol '~sym) ~exp1 ~exp2))

(defn- symbolic
  [exp pc state path]
  (match [(first exp)]
    [:INT] [(read-string (second exp)) pc state]
    [:OPER] [(read-string (second exp)) pc state]
    [:VarExp] (let [varname (get state (read-string (second exp)))]
                (assert (not= varname nil) "Variable not declared (symbolic)")
                [varname pc state])
    [:OpExp] (let [[_ exp1 oper exp2] exp
                   e1 (first (symbolic exp1 pc state path))
                   op (first (symbolic oper pc state path))
                   e2 (first (symbolic exp2 pc state path))
                   condition (z3/assert e1 op e2)]
               (if path
                 [condition (conj pc condition) state]
                 [condition (conj pc (z3/not condition)) state]))
    [:add] (let [[_ exp1 exp2] exp]
             [(arithmetic + (first (symbolic exp1 pc state path))
                          (first (symbolic exp2 pc state path)))
              pc state])
    [:sub] (let [[_ exp1 exp2] exp]
             [(arithmetic - (first (symbolic exp1 pc state path))
                          (first (symbolic exp2 pc state path)))
              pc state])
    [:mul] (let [[_ exp1 exp2] exp]
             [(arithmetic * (first (symbolic exp1 pc state path))
                          (first (symbolic exp2 pc state path)))
              pc state])
    [:ParenExp] (symbolic (second exp) pc state path)
    [:AssignExp] (if (not= (first (last exp)) :UserInput)
                   (let [[varexp bodyexp] (rest exp)
                         varname (read-string (second varexp))
                         body (first (symbolic bodyexp pc state path))]
                     [body pc (conj state {varname body})])
                   [[] pc state])
    [_] [[] pc state]))

(defn- evaluate-node
  [exp pc env state]
  (let [res (concrete exp env)
        path (:path (meta res))
        new-env (last res)]
    (if (isa? ::Error (first res))
      ::Error
      (let [s (symbolic exp pc state path)]
        (let [[new-pc new-state] (rest s)]
          (with-meta [new-pc new-env new-state] {:path path}))))))

(defn- transition
  [node path]
  (if path
    (cfg/get-t node)
    (cfg/get-f node)))

(defn- traverse
  [node pc env state]
  {:pre [(map? state) (map? env) (vector? pc)]}
  (when (cfg/node? node)
    (let [exp (.exp node)
          evaluated (evaluate-node exp pc env state)]
      (if (isa? ::Error evaluated)
        (println "Reached error state on line: " (.start node)
                 "," (.end node)
                 " for expression: " (.exp node)
                 " for: " @inputs)
        (dfs (:path (meta evaluated)) node [pc env state] evaluated)))))

(defn- dfs
  [path node old-args new-args]
  (let [[pc env state] old-args
        [new-pc new-env new-state] new-args
        exp (.exp node)]
    (if (= (first exp) :OpExp)
      (do
        (if (not (cfg/get-edge node path))
          (do (cfg/mark-edge node path)
              (traverse (transition node path) new-pc new-env new-state))
          (traverse (transition node path) pc new-env state))
        (let [negated-pc (conj pc (z3/not (last new-pc)))
              new-inputs (z3/solve negated-pc)
              _ (swap! inputs conj new-inputs)
              negated-env (conj env new-inputs)]
          (when (and (z3/check-sat negated-pc))
            (when-not (cfg/get-edge node (not path))
              (cfg/mark-edge node (not path))
              (traverse (transition node (not path))
                        negated-pc negated-env new-state)))))
      (traverse (cfg/get-t node) new-pc new-env new-state))))

(defn execute
  [node]
  ; Find all symbolic values
  ; Add them z3
  ; solve z3 to get initial values
  ; update state
  (let [sym-vars (util/findSym node)
        pc (vec (map #(z3/const % Int) sym-vars))
        env (reduce conj {} (map #(vector % (rand-int 1000)) sym-vars))
        _ (reset! inputs env)
        _ (println "start-env" env)
        state (reduce conj {} (map #(vector % %) sym-vars))]
    (traverse node pc env state)))

; (execute (cfg/build "(x := input(); if x>500 then if x<750 then error() else error() else error())"))

; (execute (cfg/build "(x := input(); y := 0; while x < 10 do (x := x+1; y := y+1); if y>5 then error() else 40)"))
;; TODO: This throws false positives
(execute (cfg/build (slurp (clojure.java.io/resource "test-programs/gcd.sec"))))
; (execute (cfg/build "(x := input(); while x<10 do x := x + 1)"))
; (execute (cfg/build "x := 5"))
