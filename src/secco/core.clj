(ns secco.core
  (:require [secco.cfg :as cfg]
            [secco.interpreter :as ip]
            [instaparse.core :as insta]
            [clojure.java.io :as io]
            [secco.viz :as viz]
            [secco.concolic :as conc]
            [secco.symbolic :as sym]
            [z3.solver :as z3]
            [secco.util :refer [time-with-return]])
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
           (sym/execute (cfg/build file))
           (clojure.string/includes? opt "-time")
           (let [n 10
                 total-time (reduce + (repeatedly n
                                                  #(last (time-with-return (conc/execute (cfg/build file))))))]
             (println "Total time: " (/ total-time n))
             (println "z3-time: " (/ @z3/z3-time n)))
           (clojure.string/includes? opt "-heur")
           (let [n 100 
                 dfs (reduce + (repeatedly n #(last (time-with-return (conc/execute (cfg/build file) '() )))))
                 bfs (reduce + (repeatedly n #(last (time-with-return (conc/execute (cfg/build file) '[] )))))
                 ]
             (println "Total time (DFS): " (/ dfs n))
             (println "Total time (BFS): " (/ bfs n)))
           (clojure.string/includes? opt "-conc")
           (conc/execute (cfg/build file))))))
