(ns secco.cfg
  [:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [clojure.core.match :refer [match]]])

(def grm
  (insta/parser (io/resource "secco.grm")
                :auto-whitespace :standard))

(defprotocol CFGNode 
  (get-t [this])
  (get-f [this])
  (set-t [this node])
  (set-f [this node])
  (print-path [this node]))

(deftype Node [nam exp ^:volatile-mutable t ^:volatile-mutable f]
  CFGNode
  (get-t [this] t)
  (get-f [this] f)
  (set-t [this node] (set! t node))
  (set-f [this node] (set! f node))
  (print-path [this node] (println node))
  (toString [_] (str "Node: <" nam ">, Exp: " exp)))

(defn parse-tree->cfg
  ([prog] (parse-tree->cfg prog []))
  ([prog path]
  (let [[exp & exps] prog]
    (match [exp]
      [:root] (let [node (parse-tree->cfg (first exps))] 
                (->Node "root" "" node node))
      [:OpExp] (->Node "oper" prog path path)
      [:UserInput] (->Node "input()" prog path path)
      [:Error] (->Node "error()" prog path path)
      [:add] (->Node "arith" prog path path)
      [:sub] (->Node "arith" prog path path)
      [:mul] (->Node "arith" prog path path)
      [:WhileExp] (let [
                         [guard body] exps 
                         gnode (parse-tree->cfg guard path)
                         bnode (parse-tree->cfg body gnode)
                       ]
                    (set-t gnode bnode)
                    gnode)
      [:INT] (->Node "int" prog path path) 
      [:VarExp] (->Node "varexp" prog path path)
      [:AssignExp] (->Node "assignexp" prog path path)
      [:IFEXP] (let [
                      [guard tru fal] exps
                      gnode (parse-tree->cfg guard)
                      tnode (parse-tree->cfg tru path)
                      fnode (parse-tree->cfg fal path)
                    ]
                  (set-t gnode tnode)
                  (set-f gnode fnode)
                 gnode)
      [:ParenExp] (parse-tree->cfg exps) 
      [:SeqExp] (reduce (fn [node x] (parse-tree->cfg x node)) 
                        (parse-tree->cfg (last exps) path)
                        (rest (reverse exps)))
      )))
  )

(defn build [program]
  (parse-tree->cfg (grm program)))

; (get-t(get-t(build "if x>2 then x else 0")))
; (get-f(get-t(build "if x>2 then x else 0")))
; (get-t(build "(2+2)*(3+2)")) ; TODO: becomes SeqExp, fix in grammar?
; (get-t(build "(0;1;2;3;4;5)"))
; (get-t(get-t(build "(0;1)")))
; (build "4+4")
; (get-t (build "%dette er en kommentar% (x:=1; if x=3 then 1 else 2)"))
; (get-t (build "x := input()"))
