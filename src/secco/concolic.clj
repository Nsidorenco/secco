(ns secco.concolic
  (:require [z3.solver :as z3]
            [secco.cfg :as cfg]
            [secco.util :as util]
            [clojure.pprint]
            [clojure.core.match :refer [match]]))

(def inputs (atom {}))
(def ^:dynamic *root* [])
(def ^:dynamic *root-env* [])
(def ^:dynamic *root-state* [])
(def ^:dynamic *root-pc* [])
(def reset (atom :none))
(def array-loop (atom []))

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
                                           [value env])
                            [:ArrayVar] (let [arr (get env (read-string (second (second exp))))
                                              [array-index _] (concrete (second (last (second exp))) env)
                                              value (get env (str arr array-index))
                                              size (loop [n 0]
                                                     (if (nil? (get env (str arr n)))
                                                       n
                                                       (recur (inc n))))]
                                          (if (< array-index size)
                                            [value env]
                                            (do
                                              (println "Symbolic out of bounds")
                                              (reset! reset :reset)
                                              [0 env]))))
                [:OpExp] (let [[exp1 oper exp2] (rest exp)
                               exp1 (first (concrete exp1 env))
                               exp2 (first (concrete exp2 env))
                               oper (first (concrete oper env))
                               res (op-exp exp1 oper exp2)]
                           (if res
                             (with-meta [[] env] {:path true})
                             (with-meta [[] env] {:path false})))
                [:ParenExp] (concrete (second exp) env)
                [:Size] (let [arr (get env (read-string (second (second (second exp)))))
                              size (loop [n 0]
                                     (if (nil? (get env (str arr n)))
                                       n
                                       (recur (inc n))))]
                          [size env])
                [:Array] (let [uid (gensym)
                               arraysize (first (concrete (second (second exp)) env))
                               array (reduce (fn [acc e] (conj acc (str uid e))) [] (range arraysize))]
                           [uid (conj env (reduce (fn [acc e] (conj acc {e 0})) {} array))])
                [:AssignExp] (if (not= (first (last exp)) :UserInput)
                               (let [[varexp body] (rest exp)
                                     varname (second (second varexp))
                                     [body env'] (concrete body env)]
                                 (if (= (first (second varexp)) :ArrayVar)
                                   (let [l_value (second (last (second (second exp))))
                                         idx (first (concrete l_value env))
                                         uid (get env (read-string varname))]
                                     [body (conj env env' {(str uid idx) body})])
                                   [body (conj env env' {(read-string varname) body})]))
                               (if (= (first (second (second exp))) :ArrayVar)
                                 (let [l_value (second (last (second (second exp))))
                                       idx (first (concrete l_value env))
                                       varname (second (second (second exp)))
                                       uid (get env (read-string varname))
                                       value (get env (read-string (str varname idx)))]
                                   [value (conj env {(str uid idx) value})])
                                 [[] env]))

                [_] [[] env])]
      (if (nil? (:path (meta res)))
        (with-meta res {:path true})
        res))))

(defmacro arithmetic
  [sym exp1 exp2]
  `(list (symbol '~sym) ~exp1 ~exp2))

(defn evaluate-index [exp env]
  (if (list? exp)
    (let [exps (map #(evaluate-index % env) (rest exp))
          e1 (first exps)
          e2 (second exps)]
      (if (and (number? e1) (number? e2))
        (apply (resolve (first exp)) exps)
        exp))
    (if (number? exp)
      exp
      (get env exp))))

(defn- symbolic
  [exp env pc state path]
  (match [(first exp)]
    [:INT] [(read-string (second exp)) pc state]
    [:OPER] [(read-string (second exp)) pc state]
    [:Error] (do (reset! reset :error) [::Error pc state])
    [:VarExp] (match [(first (second exp))]
                [:SimpleVar] (let [value (get state (read-string (second (second exp))))]
                               (if (nil? value)
                                 (do (reset! reset :error) [::Error pc state])
                                 [value pc state]))
                [:ArrayVar] (let [arr-name (read-string (second (second exp)))
                                  uid (get state arr-name)
                                  index (first (symbolic (second (last (second exp))) env pc state path))
                                  index-eval (evaluate-index index env)
                                  index-sym (evaluate-index index state)
                                  size (loop [n 0]
                                         (if (nil? (get state (str uid n)))
                                           n
                                           (recur (inc n))))
                                  new-pc (z3/assert index-sym (read-string "<") size)]
                              (do
                                (when-not (number? index-sym)
                                  (doseq [i (range (inc size))]
                                    (let [new-pc (conj pc (z3/assert index-sym (read-string "=") i))]
                                      (when (and (not (.contains pc (last new-pc)))
                                                 (not (number? index-sym))
                                                 (z3/check-sat new-pc))
                                        (swap! array-loop conj new-pc)))))
                                (if (not (< index-eval size))
                                  (do (println "Out of bounds for array" arr-name ", index: " index-eval ", size: " size)
                                      (reset! reset :halt)
                                      [[] pc state])
                                  (if (.contains pc new-pc)
                                    [(get state (str uid index-eval)) pc state]
                                    [(get state (str uid index-eval)) (conj pc new-pc) state])))))
    [:OpExp] (let [[_ exp1 oper exp2] exp
                   [e1 pc1 _] (symbolic exp1 env pc state path)
                   op (first (symbolic oper env pc state path))
                   [e2 pc2 _] (symbolic exp2 env pc1 state path)
                   condition (z3/assert e1 op e2)]
               (if path
                 [condition (conj pc2 condition) state]
                 [condition (conj pc2 (z3/not condition)) state]))
    [:Array] (let [uid (gensym)
                   arraysize (first (symbolic (second (second exp)) env pc state path))
                   arraysize (evaluate-index arraysize env)
                   ;arraysize (if (number? arraysize) arraysize (get env arraysize))
                   array (reduce (fn [acc e] (conj acc (str uid e))) [] (range arraysize))]
               [uid pc (conj state (reduce (fn [acc e] (conj acc {e 0})) {} array))])
    [:Size] (let [arr (get state (read-string (second (second (second exp)))))
                  size (loop [n 0]
                         (if (nil? (get state (str arr n)))
                           n
                           (recur (inc n))))]
              [size pc state])
    [:add] (let [[_ exp1 exp2] exp]
             [(arithmetic + (first (symbolic exp1 env pc state path))
                          (first (symbolic exp2 env pc state path)))
              pc state])
    [:sub] (let [[_ exp1 exp2] exp]
             [(arithmetic - (first (symbolic exp1 env pc state path))
                          (first (symbolic exp2 env pc state path)))
              pc state])
    [:mul] (let [[_ exp1 exp2] exp]
             [(arithmetic * (first (symbolic exp1 env pc state path))
                          (first (symbolic exp2 env pc state path)))
              pc state])
    [:ParenExp] (symbolic (second exp) env pc state path)
    [:AssignExp] (let [[varexp bodyexp] (rest exp)
                       varname (read-string (second (second varexp)))
                       [body pc state] (symbolic bodyexp env pc state path)
                       uid (get state varname)]
                   (if (not= (first (last exp)) :UserInput)
                     (if (= (first (last varexp)) :ArrayVar)
                       (let [l_value (second (last (second (second exp))))
                             idx (first (symbolic  l_value env pc state path))
                             idx2 (evaluate-index idx env)
                             evaluate-array (symbolic (second exp) env pc state path)]
                         (if (nil? (first evaluate-array))
                           (do (reset! reset :error) [::Error pc state])
                           [body pc (conj state {(str uid idx2) body})]))
                       [body pc (conj state {varname body})])
                     (if (= (first (second (second exp))) :ArrayVar)
                       (let [l_value (second (last (second (second exp))))
                             idx (first (symbolic  l_value env pc state path))]
                         [body pc (conj state {(str uid idx) (read-string (str varname idx))})])
                       [[] pc state])))
    [_] [[] pc state]))

(defn- evaluate-node
  [exp pc env state]
  (let [[res new-env :as xyz] (concrete exp env)
        path (-> xyz meta :path)
        s (symbolic exp env pc state path)]
    (with-meta (conj s new-env) {:path path})))

(defn- transition
  [node path]
  (if path
    (cfg/get-t node)
    (cfg/get-f node)))

(defn- traverse
  [node pc state env strategy]
  {:pre [(map? state) (map? env) (vector? pc)]}
  ;(clojure.pprint/pprint state)
  (reset! array-loop [])
  (if (cfg/node? node)
    (let [exp (.exp node)
          [err new-pc new-state new-env :as evaluated] (evaluate-node exp pc env state)
          path (-> evaluated meta :path)
          strategy (if-not (or (cfg/get-edge node path)
                               (cfg/get-edge node (not path)))
                     (reduce conj strategy @array-loop)
                     strategy)]
      (cfg/mark-edge node path)
      (case @reset
        :reset (let [new-inputs (conj *root-env* (z3/solve new-pc))]
                 (reset! reset :none)
                 (println "new-inputs: " new-inputs)
                 (swap! inputs conj new-inputs)
                 (recur *root* *root-pc* *root-state* new-inputs strategy))
        :halt (do
                (reset! reset :none)
                (recur [] pc state env strategy))
        (do
          (when (isa? @reset :error)
            (reset! reset :none)
            (println "Reached error state on line: " (.start node)
                     "," (.end node)
                     " for ex
                     pression: " exp
                     " for: " @inputs)
            (traverse [] pc state env strategy))
          (if (= (first exp) :OpExp)
            (if (and (not (cfg/get-edge node (not path)))
                     (not (.contains strategy (conj pc (z3/not (last new-pc)))))
                     (z3/check-sat (conj pc (z3/not (last new-pc)))))
              (recur (transition node path)
                     new-pc new-state new-env
                     (conj strategy (conj pc (z3/not (last new-pc)))))
              (recur (transition node path)
                     new-pc new-state new-env
                     strategy))
            (recur (transition node path)
                   new-pc new-state new-env
                   strategy)))))
    (when-let [new-pc (peek strategy)]
      (let [new-inputs (conj *root-env* (z3/solve new-pc))]
        (println "new-inputs: " new-inputs)
        (swap! inputs conj new-inputs)
        (recur *root* *root-pc* *root-state* new-inputs (pop strategy))))))

(defn assert-positive [pc acc]
  (if (empty? acc)
    pc
    (let [var (first (first acc))
          pc (conj pc (z3/assert var (read-string (str ">")) -1))]
      (assert-positive pc (rest acc)))))

(defn execute
  ([node] (execute node '()))
  ([node strategy]
  ; Find all symbolic values
  ; Add them z3
  ; solve z3 to get initial values
  ; update state
   (let [sym-vars (util/findSym node)
         pc (vec (map #(z3/const % Int) sym-vars))
         state (reduce conj {} (map #(vector % %) sym-vars))
         env (reduce conj {} (map #(vector % (rand-int 20)) sym-vars))
         pc (assert-positive pc state)]
     (reset! inputs env)
     (reset! array-loop [])
     (reset! reset :none)
     (binding [*root* node
               *root-pc* pc
               *root-state* state
               *root-env* env]
       (traverse node pc state env strategy))
    ; (println "z3-time: " @z3/z3-time)
     (println "it is a-okay, my dudes"))))

;(execute (cfg/build "(a:=array(4); a[2]:=input())"))
;(execute (cfg/build "(a:= array(5); x :=input(); a[x])"))
;(execute (cfg/build "(a:= array(5); x :=5; a[x])"))
;(execute (cfg/build "(a:=3;b:=a)"))
;(execute (cfg/build "x := input(); x"))
;(execute (cfg/build "(x:=array(4); x[2]:=input())"))
;(execute (cfg/build "(b := array(4); b[2]:=3+4)"))
;(execute (cfg/build "(a:=input(); if a<10 then error() else 1)"))
;(execute (cfg/build "(a:=array(4); a[2]:=input(); if a[2]>7000 then error() else 1)"))
;(execute (cfg/build "(a:=array(4); a[2]:=input(); a[3]:=2+3)"))
;(execute (cfg/build "(a:=array(4); a[2]:=input(); if a[2] = 1 then error() else 1)"))
;(execute (cfg/build "(a:=array(4); a[2]:=input(); if a[2] = 1 then error() else error())"))
;(execute (cfg/build "(x:=input(); x:=x+2; x)"))
;(execute (cfg/build "(x:=input(); if x>500 then if x<750 then error() else error() else error())"))
;(execute (cfg/build "(x := input(); y := 0; while x < 10 do (x := x+1; y := y+1) end; if y>5 then error() else 40)"))
;(execute (cfg/build (slurp (clojure.java.io/resource "test-programs/gcd.sec"))))
; (execute (cfg/build "(x := input(); while x<10 do x := x + 1)"))
; (execute (cfg/build "x := 5"))
;(execute (cfg/build "(x:=input(); if x>20 then if x<50 then error() else error() else error())"))

;(time (execute (cfg/build (slurp (clojure.java.io/resource "test-programs/bubblesort.sec")))))
;(time (execute (cfg/build (slurp (clojure.java.io/resource "test-programs/insertionsort.sec")))))
;(time (execute (cfg/build (slurp (clojure.java.io/resource "test-programs/mergesort.sec")))))

;(execute (cfg/build "(a:=array(10); x:=2; a[x+1+x]:=2)"))
;(execute (cfg/build "(a:=array(4); x:=input(); a[2])"))
;(execute (cfg/build "(a:=array(4); a[2])"))
; (execute (cfg/build "(a:=array(4); x:=2; a[x+1]:=2)"))
;(execute (cfg/build "(x:=input(); a:=array(4); a[0]:=1; a[1]:=x; a[2]:=3)"))
;(execute (cfg/build "(a:=array(4); x:=3; a[2]:=1; a[x]:=5)"))
;(execute (cfg/build "(a:=array(4); x:=input(); a[2]:=1; a[x]:=x)"))
;(execute (cfg/build "(a:=array(5); a[3]:=7; x:=input(); if a[x] > 3 then error() else 0)"))

;(execute (cfg/build "(x:=input(); a:=array(4); a[x])"))
;(execute (cfg/build "(x:=input(); a:=array(10); a[x]:=5)"))
;(execute (cfg/build "(x:=10; a:=array(x); a[x-1]:=5)"))
;(execute (cfg/build "(i:=0; while i<5 do (a:=array(i); i:=i+1) end)"))
;(execute (cfg/build "(x:=input(); a:=array(10); a[x]:=2; if a[2]=2 then error() else 1)"))
;(execute (cfg/build "(x:=input(); y:=input(); a:=array(x); a[y]:=x)"))
;(execute (cfg/build "(x:=input(); a:=array(x); a[x-1]:=2)"))
;(execute (cfg/build "(x:=input(); if x<0 then error() else 0)"))
;(execute (cfg/build "(a:=input(); b:=input(); c:=input(); if a>5 then a:=a-1 else a:=a+1; if b>5 then b:=b-1 else b:=b+1; if c>5 then c:=c-1 else c:=c+1; if a+b+c=10 then error() else 0)"))