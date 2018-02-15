(ns secco.core
  [:require [instaparse.core :as insta]
            [clojure.java.io :as io]])

(def grm
  (insta/parser (io/resource "secco.grm")
                :auto-whitespace :standard))

(defprotocol CFGNode 
  (get-t [this])
  (get-f [this])
  (set-t [this node])
  (set-f [this node]))

(deftype Node [nam exp ^:volatile-mutable t ^:volatile-mutable f]
  CFGNode
  (get-t [this] t)
  (get-f [this] f)
  (set-t [this node] (set! t node))
  (set-f [this node] (set! f node))
  (toString [_] (str "Node: <" nam ">, Exp: " exp)))

(defrecord CFG [Node])

(insta/parses grm (slurp (io/resource "test1.sec")))
(insta/parses grm (slurp (io/resource "iftest1.sec")))
(insta/visualize (insta/parse grm (slurp (io/resource "whiletest1.sec"))))
(insta/parses grm "1 +  2")
(insta/parses grm "var123 := 4")
(insta/parses grm "(1; 2; 3)")
(insta/parse grm "(1 + 2 * 5)")

(def flow_graph (CFG. {}))

(def parse-tree->cfg
  {:WhileExp (fn [tst body] 
               (let 
                 [ bodynode (->Node "Body" body [] [])
                   testnode (->Node "Test" tst bodynode [])
                 ]
                (set-t bodynode testnode) 
                (println testnode)
                (println bodynode)
                ))
   :INT read-string
   :OpExp (fn [exp1 opr exp2]
            (str exp1 opr exp2))
   :OPER read-string
   :VarExp read-string
   :AssignExp (fn [v a exp]
                (str v a exp))
   })

(insta/transform parse-tree->cfg (grm "while x < y do x := x + 1"))
(insta/transform parse-tree->sexp (grm "20 + 35"))

(def a (ref '(1 2 3)))

(ref-set a '(3 2 1))

(eval (first (insta/transform parse-tree->sexp (grm "20 - 35"))))

(defn mark
   ([exp] exp)
   ([oper, exp] `(+ ~(mark exp))))
 
(mark 1 "hej")

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
