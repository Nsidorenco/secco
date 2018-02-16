(ns secco.core
  [:require [secco.cfg :as cfg]
            [secco.interpreter :as ip]])

(ip/interpret-expression (cfg/build "4+4"))
(ip/interpret (cfg/build "while x < y do x := x + 1"))
