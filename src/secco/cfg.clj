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

(def ye-olde-parse-tree->cfg
  {:WhileExp (fn [tst body] 
               (let 
                 [ bodynode (->Node "Body" body [] [])
                   testnode (->Node "Test" tst bodynode [])
                 ]
                (set-t bodynode testnode) 
                (println testnode)
                (print-path testnode (get-t testnode))
                (print-path bodynode (get-t bodynode))
                )),
   :root (->Node "Root" [] [] [])
   })

(defn yet-another-parse-tree->cfg 
  ([prog]
  (let [ [exp & exps] prog ]
    ({:WhileExp (let
                 [ [guard body] exps
                   ;gnode (parse-tree->cfg guard)
                   ;bnode (parse-tree->cfg body gnode)
                   ]
                 (println exps)
                 (println exp)),
     :OpExp read-string,
     :root (fn [exp]
             (let [node (parse-tree->cfg exp)]
               (->Node "root" "" node node)))
     } exp)))
  ([exp path]
  ({:OpExp (fn [oper] (->Node "oper" oper path path))

   } exp)))

(defn parse-tree->cfg
  ([prog]
  (let [[exp & exps] prog]
    (match [exp]
      [:root] (let [node (parse-tree->cfg (first exps))] 
                (->Node "root" "" node node)), 
      [:OpExp] (->Node "oper" prog [] []),
      [:WhileExp] (let [
                       [guard body] exps 
                       gnode (parse-tree->cfg guard)
                       bnode (parse-tree->cfg body gnode)
                        ]
                    (set-t gnode bnode)
                    gnode),
      [:INT] read-string
      )))
  ([prog path]
  (let [[exp & exps] prog]
    (match [exp]
      [:OpExp] (->Node "oper2" prog path path) 
      [:INT] (->Node "int" prog path path)
   ))
  ))

  
(defn build [program]
  (insta/transform parse-tree->cfg (grm program)))

(get-t(get-t(parse-tree->cfg (grm "while 1<2 do 3"))))
(parse-tree->cfg (grm "4+4"))
