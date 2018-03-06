(ns secco.cfg
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [clojure.core.match :refer [match]]))

(def grm
  (insta/parser (io/resource "secco.grm")
                :auto-whitespace :standard))

(defprotocol CFGNode
  (get-t [this])
  (get-f [this])
  (set-t [this node])
  (set-f [this node])
  (print-path [this node]))

(deftype Node [nam exp ^:volatile-mutable t ^:volatile-mutable f start end]
  CFGNode
  (get-t [this] t)
  (get-f [this] f)
  (set-t [this node] (set! t node))
  (set-f [this node] (set! f node))
  (print-path [this node] (println node))
  (toString [_] (str "Node: <" nam ">, Exp: " exp)))

(defn new-node
  [nam prog t_path f_path]
  (let [start (:instaparse.gll/start-line (meta prog))
        end (:instaparse.gll/end-line (meta prog))]
    (->Node nam prog t_path f_path start end)))

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
       [:VarExp] (new-node "varexp" prog path path)
       [:AssignExp] (new-node "assignexp" prog path path)
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

(defn build [program]
  (let [ast (insta/add-line-and-column-info-to-metadata program (grm program))
        tree (parse-tree->cfg ast)]
    (if (insta/failure? ast)
      (throw (Exception. "Invalid test program"))
      tree)))

; (get-f(get-t(build "if x>2 then x else 0")))
; (get-t(build "(2+2)*(3+2)")) ; TODO: becomes SeqExp, fix in grammar?
; (get-t (build "(0;1;2;3;4;5)"))
; (get-t(get-t(build "(0;1)")))
; (.start (get-t (build "4+4")))
; (get-t (build "%dette er en kommentar% (x:=1; if x=3 then 1 else 2)"))
; (get-t (build "x := input()"))
; (def file (slurp (clojure.java.io/resource "test-programs/iftest2.sec")))
