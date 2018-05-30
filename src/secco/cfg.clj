(ns secco.cfg
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [clojure.core.match :refer [match]]))

(def node-count (atom 0))

(def grm
  (insta/parser (io/resource "secco.grm")
                :auto-whitespace :standard))

(defprotocol CFGNode
  (get-edge [this boolean])
  (mark-edge [this boolean])
  (get-t [this])
  (get-f [this])
  (set-t [this node])
  (set-f [this node])
  (print-path [this node]))

(deftype Node [nam exp ^:volatile-mutable t ^:volatile-mutable f start end
               ^:volatile-mutable flag-t ^:volatile-mutable flag-f]
  CFGNode
  (get-edge [this b] (if b flag-t flag-f))
  (mark-edge [this b] (if b (set! flag-t true) (set! flag-f true)))
  (get-t [this] t)
  (get-f [this] f)
  (set-t [this node] (set! t node))
  (set-f [this node] (set! f node))
  (print-path [this node] (println node))
  (toString [_] (str "Node: <" nam ">, Exp: " exp)))

(defn next-node
  [n node]
  (loop [node node itr 0]
    (if (= itr n)
      node
      (recur (get-t node) (inc itr)))))

(defn node? [maybe-node]
  (satisfies? CFGNode maybe-node))

(defn new-node
  [nam prog t_path f_path]
  (let [start (:instaparse.gll/start-line (meta prog))
        end (:instaparse.gll/end-line (meta prog))]
    (swap! node-count inc)
    (->Node nam prog t_path f_path start end false false)))

(defn find-value [exp]
  (match [(first exp)]
    [:INT] [:INT (second exp)]
    [:SimpleVar] [:VarExp [:SimpleVar (second exp)]]
    [:VarExp] (recur (second exp))
    [:lvalue] (recur (second exp))))

(defn parse-tree->cfg
  ([prog] (parse-tree->cfg prog []))
  ([prog path]
   (let [[exp & exps] prog]
     (match [exp]
       [:root] (let [node (parse-tree->cfg (first exps))]
                 (new-node "root" "" node node))
       [:OpExp] (new-node "oper" prog path path)
       [:UserInput] (new-node "input()" prog path path)
       [:Error] (new-node "error()" prog path path)
       [:add] (new-node "arith" prog path path)
       [:sub] (new-node "arith" prog path path)
       [:mul] (new-node "arith" prog path path)
       [:WhileExp] (let [[guard body] exps
                         gnode (parse-tree->cfg guard path)
                         bnode (parse-tree->cfg body gnode)]
                     (set-t gnode bnode)
                     gnode)
       [:INT] (new-node "int" prog path path)
       [:Size] (new-node "size" prog path path)
       [:VarExp] (new-node "varexp" prog path path)
       [:AssignExp] (let [[lvalue value] exps]
                      (if (= :IFEXP (first lvalue))
                        (let [[_ op t f] lvalue
                              gnode (parse-tree->cfg op path)
                              tnode (parse-tree->cfg [:AssignExp t value] path)
                              fnode (parse-tree->cfg f path)]
                          (set-t gnode tnode)
                          (set-f gnode fnode)
                          gnode)
                        (if (= :IFEXP (first value))
                          (let [[_ op t f] value
                                gnode (parse-tree->cfg op path)
                                tnode (parse-tree->cfg [:AssignExp lvalue t] path)
                                fnode (parse-tree->cfg f path)]
                            (set-t gnode tnode)
                            (set-f gnode fnode)
                            gnode)
                          (new-node "assignexp" prog path path))))
       [:Array] (new-node "arrayexp" prog path path)
       [:IFEXP] (let [[guard tru fal] exps
                      gnode (parse-tree->cfg guard)
                      tnode (parse-tree->cfg tru path)
                      fnode (parse-tree->cfg fal path)]
                  (set-t gnode tnode)
                  (set-f gnode fnode)
                  gnode)
       [:ParenExp] (parse-tree->cfg exps)
       [:SeqExp] (reduce (fn [node x] (parse-tree->cfg x node))
                         (parse-tree->cfg (last exps) path)
                         (rest (reverse exps)))))))

(defn transform-ast
  [ast]
  (insta/transform
   {:VarExp (fn [param]
              (let [[t id lvalue :as exp] param]
                (if (= t :ArrayVar)
                  [:IFEXP
                   [:OpExp
                    (find-value lvalue)
                    [:OPER "<"]
                    [:Size [:VarExp [:SimpleVar id]]]]
                   [:VarExp param]
                   [:Error "error()"]]
                  [:VarExp param])))}
   ast))

(defn build [program]
  (let [ast (->> program
                 grm
                 (insta/add-line-and-column-info-to-metadata program)
                 transform-ast)]
    (reset! node-count 0)
    (if (insta/failure? ast)
      (do (println ast)
          (throw (Exception. "error parsing input")))
      (parse-tree->cfg ast))))

; (next-node 2 (build "a[x]:=input()"))
(next-node 1 (build "x:=a[x]"))
; (println (grm "if a[x] < size(a) then a[x] else error()"))
;(println (grm "(x := 3; x := 2+x; x)"))
;(println (grm "(a:=array(4); x:=input(); a[2]:=x; if a[2] = 7 then error() else 1)"))
;(println (get-f (get-t (get-t (build "(a:=array(4); a[0])")))))
;(println (next 2 (build "(a:=array(4); a[0])")))
;(clojure.pprint/pprint (grm "(a:=array(5); a[3]:=7; x:=input(); if a[x] > 3 then error() else 0)"))

; (clojure.pprint/pprint (transform-ast (grm "(a:=array(5); a[3]:=7; x:=input(); if a[x] > 3 then error() else 0)")))
