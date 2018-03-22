(ns secco.concolic
  (:require [z3.solver :as z3]
            [secco.cfg :as cfg]
            [clojure.core.match :refer [match]]))

(defn- op-exp
  [exp1 oper exp2]
  (if (= "!=" (str oper))
    (not (oper exp1 exp2))
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
                [:VarExp] (let [value (get env (read-string (second exp)))]
                            (assert (not= value nil)
                                    "Variable not declared (concrete)")
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
                [:AssignExp] (if (not= (first (last exp)) :UserInput)
                               (let [[varexp bodyexp] (rest exp)
                                     varname (read-string (second varexp))
                                     body (first (concrete bodyexp env))]
                                 [body (conj env {varname body})])
                               [[] env])
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
    [:Error] ::Error
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
    (let [s (symbolic exp pc state path)]
      (if (vector? s)
        (let [[new-pc new-state] (rest s)]
          (with-meta [new-pc new-env new-state] {:path path}))
        ::Error))))

(defn- transition
  [node path]
  (if path
    (cfg/get-t node)
    (cfg/get-f node)))

(defn- traverse
  [node pc env state]
  {:pre [(map? state) (map? env) (vector? pc)]}
  ; Concrete execution should detect errors
  ; Symbolic should not branch non-deterministically 
  ; Recursive call first, then pc negation call
  (when (cfg/node? node)
    (println (.exp node))
    (let [exp (.exp node)
          evaluated (evaluate-node exp pc env state)]
      (if (vector? evaluated)
        (let [[new-pc
               new-env
               new-state] evaluated
              path (:path (meta evaluated))]
          (if (= (first exp) :OpExp)
            (do (traverse (transition node path) new-pc new-env new-state)
                (let [negated-pc (conj pc (z3/not (last new-pc)))
                      negated-env (conj env (z3/solve negated-pc))]
                  (when (z3/check-sat negated-pc)
                    (println "env: " negated-env)
                    (recur (transition node (not path))
                           negated-pc negated-env new-state))))
            (recur (cfg/get-t node) new-pc new-env new-state)))
        (println "Reached error state on line: " (.start node)
                 "," (.end node)
                 " for expression: " (.exp node)
                 " for: " env)))))

(defn findSym
  ([node] (findSym node [] #{}))
  ([node visited acc]
   (if (cfg/node? node)
     (let [exp (.exp node)]
       (if (and (= (first exp) :AssignExp)
                (= (first (last exp)) :UserInput))
         (recur (cfg/get-t node) (conj visited node) (conj acc (-> exp
                                                                   second
                                                                   second
                                                                   read-string)))
         (when-not (some (partial identical? node) visited)
           (do (findSym (cfg/get-t node) (conj visited node) acc)
               (recur (cfg/get-f node) (conj visited node) acc)))))
     acc)))

(defn execute
  [node]
  ; Find all symbolic values
  ; Add them z3
  ; solve z3 to get initial values
  ; update state
  (let [sym-vars (findSym node)
        pc (vec (map #(z3/const % Int) sym-vars))
        env (reduce conj {} (map #(vector % (rand-int 1000)) sym-vars))
        _ (println "start-env" env)
        state (reduce conj {} (map #(vector % %) sym-vars))]
    (traverse node pc env state)))

; (execute (cfg/build "(x := input(); if x>500 then if x<750 then error() else 21 else 42)"))

(execute (cfg/build "(x := input(); while x < 10 do x := x+1)"))
; (execute (cfg/build "(x := input(); x := x + 1)"))
; (execute (cfg/build "(x := 5; x := x + 1)"))
