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
   (try
     (-> (slurp (file-path prog))
         (cfg/build)
         (ip/interpret))
     (catch Exception ex
       (println "File could not be read or was not a valid secco program"))))
  ([prog opt]
   (let [file (slurp (file-path prog))]
     (cond (clojure.string/includes? opt "-cfg")
           (viz/visualize (viz/graphic (cfg/build file)))
           (clojure.string/includes? opt "-ast")
           (insta/visualize (cfg/grm file))
           (clojure.string/includes? opt "-sym")
           (do (sym/model (cfg/build file))
               (Thread/sleep 4000)
               (System/exit 0))))))
