(ns secco.core
  [:require [secco.cfg :as cfg]])

(cfg/build "while x < y do x := x + 1")
