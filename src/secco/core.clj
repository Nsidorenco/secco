(ns secco.core
  [:require [instaparse.core :as insta]
            [clojure.java.io :as io]])

(def grm
  (insta/parser (io/resource "secco.grm")
                :auto-whitespace :standard))

(insta/parses grm (slurp (io/resource "test1.sec")))
(insta/parses grm (slurp (io/resource "iftest1.sec")))
(insta/visualize (insta/parses grm (slurp (io/resource "whiletest1.sec"))))
(insta/parses grm "1 +  2")
(insta/parses grm "var123 := 4")
(insta/parses grm "(1; 2; 3)")

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
