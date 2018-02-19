(ns secco.viz
  [:require [clojure.core.match :refer [match]]
   :require [secco.cfg :as cfg]])

(try
  (require '[rhizome.viz :as r])
  (catch Exception e
    (println "Needs rhizome.viz")))

(def graph
  {:a [:b]
   :b []})

(defn visualize [graph] (r/view-graph (keys graph) graph
             :node->descriptor (fn [n] {:label n})))

(def graph (atom {}))

(defn build-tree [node]
  (if (instance? secco.cfg.CFGNode node)
    (match [(.nam node)]
           ["root"] (let []
                      (swap! graph conj {(read-string (.nam node)) [(build-tree (get-t node))]})
                      )
           ["oper"] (let [
                          [_ exp1 oper exp2] (.exp node)
                          nam (read-string (.nam node))
                          ex (str (second exp1) (second oper) (second exp2))
                          ]
                      (swap! graph conj {ex [(build-tree (get-t node)) (build-tree (get-f node))]})
                     ex))
    :end))

(reset! graph {})
(visualize (build-tree (cfg/build "3<4")))
(println @graph)
(println labels)
;(visualize graph)
