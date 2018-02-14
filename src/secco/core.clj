(ns secco.core
  [:require [instaparse.core :as insta]
            [clojure.java.io :as io]])

(def grm
  (insta/parser (io/resource "secco.grm")))

(grm (slurp (io/resource "test1.sec")))
(grm (slurp (io/resource "iftest1.sec")))
(grm (slurp (io/resource "whiletest1.sec")))
(grm "1 +  2")
(grm "var123 := 4")
(grm "(1; 2; 3)")

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
