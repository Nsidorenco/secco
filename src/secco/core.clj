(ns secco.core
  (:require [secco.cfg :as cfg]
            [secco.interpreter :as ip]
            [instaparse.core :as insta]
            [clojure.java.io :as io]
            [secco.viz :as viz]
            [secco.symbolic :as sym])
  (:gen-class))

(defn file-path
  [prog]
  (io/resource (str "test-programs/" prog)))

(defn -main
  "Main function"
  ([prog]
   (-> (slurp (file-path prog))
       (cfg/build)
       (ip/interpret)))
  ([prog opt]
   (let [file (slurp (file-path prog))]
     (cond (clojure.string/includes? opt "-cfg")
           (viz/visualize (viz/graphic (cfg/build file)))
           (clojure.string/includes? opt "-ast")
           (insta/visualize (cfg/grm file))
           (clojure.string/includes? opt "-sym")
           (sym/execute (cfg/build file))))))
