(ns secco.concolic
  (:require [z3.solver :as z3]
            [secco.cfg :as cfg]
            [secco.util :as util]
            [clojure.core.match :refer [match]]))

(def inputs (atom {}))
(def ^:dynamic *root* [])
(def ^:dynamic *root-env* [])
(def ^:dynamic *root-state* [])
(def ^:dynamic *root-pc* [])

(defn- op-exp
  [exp1 oper exp2]
  (if (= "!=" (str oper))
    (not= exp1 exp2)
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
                [:VarExp] (match [(first (second exp))]
                                 [:SimpleVar] (let [value (get env (read-string (second (second exp))))]
                                                (assert (not= value nil) "Variable not declared")
                                                [value env])
                                 [:ArrayVar] (let [arr (get env (second (second exp)))
                                                   array-index (read-string (second (last (second exp))))]
                                               [(nth arr array-index) env]))
                [:OpExp] (let [[exp1 oper exp2] (rest exp)
                               exp1 (first (concrete exp1 env))
                               exp2 (first (concrete exp2 env))
                               oper (first (concrete oper env))
                               res (op-exp exp1 oper exp2)]
                           (if res
                             (with-meta [[] env] {:path true})
                             (with-meta [[] env] {:path false})))
                [:ParenExp] (concrete (second exp) env)
                [:Array] (let [arraysize (read-string (second (second exp)))]
                           [(vec (repeat arraysize nil)) (vec (repeat arraysize nil))])
                [:AssignExp] (if (not= (first (last exp)) :UserInput)
                               (let [[varexp body] (rest exp)
                                     varname (second (second varexp))
                                     body (first (concrete body env))]
                                 (if (= (first (second varexp)) :ArrayVar)
                                   (let [arr (get env (read-string varname))
                                         array-index (read-string (second (last (second varexp))))]
                                     [body (conj env {(read-string varname) (assoc arr array-index body)})])
                                   [body (conj env {(read-string varname) body})]))
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
    [:Error] [::Error pc state]
    [:VarExp] (match [(first (second exp))]
                     [:SimpleVar] (let [value (get state (read-string (second (second exp))))]
                                    (assert (not= value nil) "Variable not declared")
                                    [value pc state])
                     [:ArrayVar] (let [arr (get state (second (second exp)))
                                       array-index (read-string (second (last (second exp))))]
                                   [(nth arr array-index) pc state]))
    [:OpExp] (let [[_ exp1 oper exp2] exp
                   e1 (first (symbolic exp1 pc state path))
                   op (first (symbolic oper pc state path))
                   e2 (first (symbolic exp2 pc state path))
                   condition (z3/assert e1 op e2)]
               (if path
                 [condition (conj pc condition) state]
                 [condition (conj pc (z3/not condition)) state]))
    [:Array] (let [arraysize (read-string (second (second exp)))]
               [(vec (repeat arraysize nil)) pc state])
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
                           varname (read-string (second (second varexp)))
                           body (first (symbolic bodyexp pc state path))]
                       (if (= (first (last varexp)) :ArrayVar)
                         (let [arr (get state varname)
                               array-index (read-string (second (last (second varexp))))]
                           [(assoc arr array-index body) pc (conj state {varname (assoc arr array-index body)})])
                         [body pc (conj state {varname body})]))
                     [[] pc state])
    [_] [[] pc state]))

(defn- evaluate-node
  [exp pc env state]
  (let [res (concrete exp env)
        new-env (last res)
        path (-> res meta :path)
        s (symbolic exp pc state path)]
    (with-meta (conj s new-env) {:path path})))

(defn- transition
  [node path]
  (if path
    (cfg/get-t node)
    (cfg/get-f node)))

(defn- traverse
  [node pc state env strategy]
  {:pre [(map? state) (map? env) (vector? pc)]}
  (if (cfg/node? node)
    (let [exp (.exp node)
          [err new-pc new-state new-env :as evaluated] (evaluate-node exp pc env state)
          path (-> evaluated meta :path)]
      (cfg/mark-edge node path)
      (when (isa? err ::Error)
        (println "Reached error state on line: " (.start node)
                 "," (.end node)
                 " for expression: " exp
                 " for: " @inputs))
      (if (= (first exp) :OpExp)
       (if (and (z3/check-sat (conj pc (z3/not (last new-pc))))
                (not (cfg/get-edge node (not path)))
                (not (.contains strategy (conj pc (z3/not (last new-pc))))))
          (recur (transition node path)
                 new-pc new-state new-env
                 (conj strategy (conj pc (z3/not (last new-pc)))))
          (recur (transition node path)
                 new-pc new-state new-env
                 strategy))
       (recur (transition node path)
              new-pc new-state new-env
              strategy)))
    (when-let [new-pc (peek strategy)]
      (let [new-inputs (conj *root-env* (z3/solve new-pc))]
        (swap! inputs conj new-inputs)
        (recur *root* *root-pc* *root-state* new-inputs (pop strategy))))))

(defn execute
  [node]
  ; Find all symbolic values
  ; Add them z3
  ; solve z3 to get initial values
  ; update state
  (let [sym-vars (util/findSym node)
        pc (vec (map #(z3/const % Int) sym-vars))
        state (reduce conj {} (map #(vector % %) sym-vars))
        env (reduce conj {} (map #(vector % (rand-int 1000)) sym-vars))]
    (reset! inputs env)
    (binding [*root* node
              *root-pc* pc
              *root-state* state
              *root-env* env]
      (traverse node pc state env (clojure.lang.PersistentQueue/EMPTY)))
    (println "it is a-okay, my dudes")))

;(execute (cfg/build ""))
;(execute (cfg/build "x := input(); x"))
;(execute (cfg/build "x := array(4)"))
;(execute (cfg/build "(a:=array(4); a[2]:=4)"))
;(execute (cfg/build "(a:=array(4); a[2]:=input(); if a[2] = 1 then error() else 1)"))
;(execute (cfg/build "(a:=array(4); a[2]:=3; if a[2] = 1 then error() else 1)"))
;(execute (cfg/build "(x:=input(); x:=x+2; x)"))
;(execute (cfg/build "x:=input(); if x>500 then if x<750 then error() else error() else error()"))
; (execute (cfg/build "(x := input(); y := 0; while x < 10 do (x := x+1; y := y+1); if y>5 then error() else 40)"))
; (execute (cfg/build (slurp (clojure.java.io/resource "test-programs/gcd.sec"))))
; (execute (cfg/build "(x := input(); while x<10 do x := x + 1)"))
; (execute (cfg/build "x := 5"))