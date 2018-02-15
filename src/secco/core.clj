(ns secco.core
  [:require [instaparse.core :as insta]
            [clojure.java.io :as io]])

(def grm
  (insta/parser (io/resource "secco.grm")
                :auto-whitespace :standard))

(insta/parses grm (slurp (io/resource "test1.sec")))
(insta/parses grm (slurp (io/resource "iftest1.sec")))
(insta/visualize (insta/parse grm (slurp (io/resource "whiletest1.sec"))))
(insta/parses grm "1 +  2")
(insta/parses grm "var123 := 4")
(insta/parses grm "(1; 2; 3)")
(insta/parse grm "(1 + 2 * 5)")

(def parse-tree->sexp
  {:OpExp (fn [exp1, oper, exp2]
            `(~oper ~exp1 ~exp2)),
   :OPER read-string,
   :INT read-string})

(insta/transform parse-tree->sexp (grm "20 + 35"))
(eval (first (insta/transform parse-tree->sexp (grm "20 - 35"))))

(defn mark
   ([exp] exp)
   ([oper, exp] `(+ ~(mark exp))))
 
(mark 1 "hej")

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
