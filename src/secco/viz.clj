(ns secco.viz
  [:require [clojure.core.match :refer [match]]
   :require [secco.cfg :as cfg]])

(try
  (require '[rhizome.viz :as r])
  (catch Exception e
    (println "Needs rhizome.viz")))

(def VALID-CHARS
  (map char (concat (range 97 123)
                   ;(range 48 58)
                   )))

(def graph
  {:a [:b]
   :b []})

(def labels (atom {}))
(defn visualize [graph] (r/view-graph (keys graph) graph
             :node->descriptor (fn [n] {:label (get @labels n)})))

(def graph (atom {}))

(defn build-tree 
  ([node] (build-tree node 0))
  ([node counter]
   (if (instance? secco.cfg.CFGNode node)
     (let [
           getsym (str(nth VALID-CHARS counter))
           ]
       (match [(.nam node)]
              ["root"] (let [ex (read-string (.nam node))]
                         (swap! labels conj {getsym ex})
                         (swap! graph conj {getsym [(build-tree (get-t node) (+ counter 1))]})
                         getsym)
              ["oper"] (let [
                             [_ exp1 oper exp2] (.exp node)
                             ex (str (second exp1) (second oper) (second exp2))
                             exist (get @graph ex) ]
                         (if (= exist nil)
                           ((swap! labels conj {getsym ex})
                           (swap! graph conj {getsym [(build-tree (get-t node) (+ counter 1)) (build-tree (get-f node) (+ counter 2))]}))
                           ())
                         getsym)
              ["int"]  (let [
                             ex (second (.exp node))
                             ]
                         (swap! labels conj {getsym ex})
                         (swap! graph conj {getsym [(build-tree (get-t node) (+ counter 1))]}) 
                         getsym)
              ["varexp"] (let [
                               ex (second (.exp node))
                               ]
                           (swap! labels conj {getsym ex})
                           (swap! graph conj {getsym [(build-tree (get-t node) (+ counter 1))]})
                           getsym)
              ["assignexp"] (let [
                                  [_ varexp body] (.exp node)
                                  ex (str (second varexp) ":=" (second body))
                                  og (.exp node)
                                  ]
                              (swap! labels conj {getsym ex})
                              (swap! graph conj {getsym [(build-tree (get-t node) (+ counter 1))]})
                              getsym)
              ))
     :end)))

(defn graphic [node]
  (reset! graph {})
  (reset! labels {})
  (build-tree node)
  @graph)

; TODO: stop uending loops
(println @graph)
(println @labels)
(visualize (graphic (cfg/build "x:=42")))
(visualize (graphic (cfg/build "(x := 0;y := 1; if x < y then y := x +1 else x)")))
(visualize (graphic (cfg/build "while x < 10 do x := x + 1")))
;(visualize graph)
