(ns secco.cfg
  (:require [instaparse.core :as insta]
            [clojure.java.io :as io]
            [clojure.core.match :refer [match]]))

(def node-count (atom 0))

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

(defn node? [maybe-node]
  (satisfies? CFGNode maybe-node))

(defn new-node
  [nam prog t_path f_path]
  (let [start (:instaparse.gll/start-line (meta prog))
        end (:instaparse.gll/end-line (meta prog))]
    (swap! node-count inc)
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
  (let [ast (insta/add-line-and-column-info-to-metadata program (grm program))]
    (reset! node-count 0)
    (if (insta/failure? ast)
      (do (println ast)
          (throw (Exception. "error parsing input")))
      (parse-tree->cfg ast))))
