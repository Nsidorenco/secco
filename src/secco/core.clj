(ns secco.core
  (:require [secco.cfg :as cfg]
            [secco.interpreter :as ip]
            [instaparse.core :as insta]
            [clojure.java.io :as io]
            [secco.viz :as viz]
            [secco.symbolic :as sym])
  (:gen-class))

; (ip/interpret (cfg/build "4+4"))
; (ip/interpret (cfg/build "while x < y do x := x + 1"))

;(viz/visualize(viz/graphic (cfg/build (slurp (io/resource "whiletest4.sec")))))
(defn file-path
  [prog]
  (io/resource (str "test-programs/" prog)))

(defn -main
  "Main function"
  ([prog]
   (try
     (-> (slurp (file-path prog))
         (cfg/build)
         (ip/interpret))
     (catch Exception ex
       (println "File could not be read or was not a valid secco program"))))
  ([prog opt]
   (let [graph (cfg/build (slurp (file-path prog)))]
     (cond (clojure.string/includes? opt "-cfg")
           (viz/visualize (viz/graphic graph))
           (clojure.string/includes? opt "-ast")
           (insta/visualize graph)
           (clojure.string/includes? opt "-sym")
           (sym/model graph)))))
