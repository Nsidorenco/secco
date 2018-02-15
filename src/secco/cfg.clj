(ns secco.cfg
  [:require [instaparse.core :as insta]
            [clojure.java.io :as io]])

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

(def parse-tree->cfg
  {:WhileExp (fn [tst body] 
               (let 
                 [ bodynode (->Node "Body" body [] [])
                   testnode (->Node "Test" tst bodynode [])
                 ]
                (set-t bodynode testnode) 
                (println testnode)
                (print-path testnode (get-t testnode))
                (print-path bodynode (get-t bodynode))
                ))
   :INT read-string
   :OpExp (fn [exp1 opr exp2]
            (str exp1 opr exp2))
   :OPER read-string
   :VarExp read-string
   :AssignExp (fn [v a exp]
                (str v a exp))
   })

(defn build 
  [program]
  (insta/transform parse-tree->cfg (grm program)))
