(ns secco.core
  [:require [secco.cfg :as cfg]
            [secco.interpreter :as ip]
            [instaparse.core :as insta]
            [clojure.java.io :as io]
            [secco.viz :as viz]]
  (:gen-class))

; (ip/interpret (cfg/build "4+4"))
; (ip/interpret (cfg/build "while x < y do x := x + 1"))

;(viz/visualize(viz/graphic (cfg/build (slurp (io/resource "whiletest4.sec")))))

(defn -main
  "Main function"
  ([prog]
  (-> (slurp (io/resource prog))
      (cfg/build)
      (ip/interpret)))
  ([prog opt]
   (cond (clojure.string/includes? opt "-cfg")
         (viz/visualize(viz/graphic (cfg/build (slurp (io/resource prog)))))
         (clojure.string/includes? opt "-ast")
         (insta/visualize (cfg/grm (slurp (io/resource prog)))))
   (-main prog)
   ))
