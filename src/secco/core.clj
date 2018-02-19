(ns secco.core
  [:require [secco.cfg :as cfg]
            [secco.interpreter :as ip]
            [clojure.java.io :as io]]
  (:gen-class))

; (ip/interpret (cfg/build "4+4"))
; (ip/interpret (cfg/build "while x < y do x := x + 1"))

(defn -main
  "Main function"
  [prog]
  (-> (slurp (io/resource prog))
      (cfg/build)
      (ip/interpret)))
