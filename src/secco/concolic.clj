(ns secco.concolic
  (:require [z3.solver :as z3]
            [clojure.core.match :refer [match]]))

(defn- concrete
  [exp env]
  (let [arithmetic (fn [exp oper]
                     (let [[_ exp1 exp2] exp]
                       (interpret-expression exp1)
                       (let [exp1 @res]
                         (interpret-expression exp2)
                         (let [exp2 @res]
                           (reset! res (oper exp1 exp2))))))]
    (match [(first exp)]
      [:INT] [(read-string (second exp)) env]
      [:OPER] [(read-string (second exp)) env]
      [:add] (arithmetic exp +)
      [:sub] (arithmetic exp -)
      [:mul] (arithmetic exp *)
      [:VarExp] (let [varname (get env (second exp))]
                  (assert (not= varname nil) "Variable not declared")
                  [varname env])
      [:OpExp] (let [[exp1 oper exp2] (rest exp)]
                 (let [exp1 (first (concrete exp1 env))
                       exp2 (first (concrete exp2 env))
                       oper (first (concrete oper env))]
                   (if (= "!=" (str oper))
                     (with-meta [[] env] {:path false})
                     (with-meta [[] env] {:path true}))))
      [:ParenExp] (recur (second exp) env)
      [:AssignExp] (let [[varexp body] (rest exp)
                         varname (second varexp)
                         body (concrete body env)]
                     [body (conj env {varname body})])
      [_] [[] env])))

(defmacro arithmetic
  [sym exp1 exp2]
  `(list (symbol '~sym) ~exp1 ~exp2))

(defn- symbolic
  [exp pc state path]
  (match [(first exp)]
    [:INT] [(read-string (second exp)) pc state]
    [:OPER] [(read-string (second exp)) pc state]
    [:Error] (println "error")
    [:VarExp] (let [varname (get state (second exp))]
                (assert (not= varname nil) "Variable not declared")
                [varname pc state])
    [:OpExp] (let [[_ exp1 oper exp2] exp
                   e1 (first (sym-exp exp1))
                   op (first (sym-exp oper))
                   e2 (first (sym-exp exp2))
                   condition (z3/assert e1 op e2)]
               (if path
                 [condition (conj pc condition) state]
                 [condition (conj pc (z3/not condition)) state]))
    [:add] (let [[_ exp1 exp2] exp]
             [(arithmetic + (first (sym-exp exp1)) (first (sym-exp exp2))) pc state])
    [:sub] (let [[_ exp1 exp2] exp]
             [(arithmetic - (first (sym-exp exp1)) (first (sym-exp exp2))) pc state])
    [:mul] (let [[_ exp1 exp2] exp]
             [(arithmetic * (first (sym-exp exp1)) (first (sym-exp exp2))) pc state])
    [:AssignExp] (let [[_ varexp bodyexp] exp]
                   varname (second varexp)
                   body (first (sym-exp bodyexp))
                   [body pc (conj state {varname body})])
    [_] ""))

(defn- evaluate-node
  [exp env pc state]
  (let [res (concrete exp env)
        path (:path (meta res))
        new-env (last res)]
    (if-let [s (vector? (symbolic exp pc state path))]
      (let [[new-pc new-state] (rest s)]
        (with-meta [new-pc new-env new-state] {:path path}))
      ::Error)))

(defn- traverse
  [node pc env state]
  ; Concrete execution should detect errors
  ; Symbolic should not branch non-deterministically 
  ; Recursive call first, then pc negation call
  (when (instance? secco.cfg.CFGNode node)
    (let [exp (.exp node)
          [path
           new-pc
           new-env
           new-state] (evaluate exp pc env state)]
      (if (= (first exp) :OpExp)
        ; What branch do we jump to?
        (do (traverse ??? new-pc new-env new-state)
            (let [negated-pc (conj (pop new-pc) (z3/not (last new-pc)))
                  negated-env (z3/solve negated-pc)]
              (when (z3/check-sat negated-pc)
                (recur ??? negated-pc negated-env new-state))))
        (recur (cfg/get-t node) new-pc new-env new-state)))))

(defn execute
  [node]
  ; Find all symbolic values
  ; Add them z3
  ; solve z3 to get initial values
  ; update state
  (let [pc (declare-symbolic)
        env (z3/solve pc)
        state {}]
    (traverse node pc state)))
