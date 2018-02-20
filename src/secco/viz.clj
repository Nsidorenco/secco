(ns secco.viz
  [:require [clojure.core.match :refer [match]]
   :require [secco.cfg :as cfg]])

(try
  (require '[rhizome.viz :as r])
  (catch Exception e
    (println "Needs rhizome.viz")))

(def alphabet 
  (map char (range 97 123)))

(defn subsets [n items]
(cond
    (= n 0) '(())
    (empty? items) '()
    :else (concat (map
                    #(cons (first items) %)
                    (subsets (dec n) (rest items)))
                  (subsets n (rest items)))))

(def symbols (map (fn [x] (keyword (clojure.string/join x))) (subsets 3 alphabet)))

(def g
    {:a [:b :c]
     :b [:c]
     :c [:a]})

(def g-edges
  {:a {:b :makes
       :c :takes}
   :b {:c :takes}
   :c {:a :makes}})

(def labels (atom {}))
(def edges (atom {}))

(defn visualize [graph] (r/view-graph (keys graph) graph
             :node->descriptor (fn [n] {:label (get @labels n)})
             :edge->descriptor (fn [src dst] {:label (dst (src @edges))})
             ))

(def graph (atom {}))

(defn pretty [body] 
  (match [(first body)]
         [:VarExp] (second body)
         [:INT] (second body)
         [:OPER] (second body)
         [:OpExp] (let [[_ exp1 oper exp2] body] 
                    (str (pretty exp1) (pretty oper) (pretty exp2)))
         [_] body))

(defn build-tree 
  ([node] (build-tree node 0 {}))
  ([node counter mark]
   (if (instance? secco.cfg.CFGNode node)
     (let [
           getsym (nth symbols counter)
           ]
       (match [(.nam node)]
              ["root"] (let [ex (read-string (.nam node))
                             tnode (build-tree (cfg/get-t node) (+ counter 1) mark)]
                         (swap! labels conj {getsym "start"})
                         (swap! graph conj {getsym [tnode]})
                         getsym)
              ["oper"] (let [
                             [_ exp1 oper exp2] (.exp node)
                             ex (str (second exp1) (second oper) (second exp2))
                             tnode (when (not (contains? mark node)) 
                                     (build-tree (cfg/get-t node) (+ counter 1) (conj mark {node getsym})))
                             fnode (if (not (contains? mark node)) 
                                     (build-tree (cfg/get-f node) (+ counter 256) (conj mark {node getsym}))
                                     :end)
                             ]
                         (if (not(contains? mark node))
                           (let [] (swap! labels conj {getsym ex})
                            (swap! edges conj {getsym {tnode "true" fnode "false"}})
                            (swap! graph conj {getsym [tnode fnode]})
                           getsym)
                           (get mark node)))
              ["int"]  (let [
                             ex (second (.exp node))
                             ]
                         (swap! labels conj {getsym ex})
                         (swap! graph conj {getsym [(build-tree (cfg/get-t node) (+ counter 1) mark)]}) 
                         getsym)
              ["varexp"] (let [
                               ex (second (.exp node))
                               ]
                           (swap! labels conj {getsym ex})
                           (swap! graph conj {getsym [(build-tree (cfg/get-t node) (+ counter 1) mark)]})
                           getsym)
              ["assignexp"] (let [
                                  [_ varexp body] (.exp node)
                                  ex (str (second varexp) ":=" (pretty body))
                                  og (.exp node)
                                  ]
                              (swap! labels conj {getsym ex})
                              (swap! graph conj {getsym [(build-tree (cfg/get-t node) (+ counter 1) mark)]})
                              getsym)
              ))
     :end)))

(defn graphic [node]
  (reset! graph {:end []})
  (reset! labels {:end "end"})
  (reset! edges {:end {}})
  (build-tree node)
  @graph)

; TODO: stop uending loops
(println @graph)
(println @labels)
(println @edges)
(visualize (graphic (cfg/build "x:=42")))
(visualize (graphic (cfg/build "(x := 0;y := 1; if x < y then if y < 2 then 0 else 1 else x)")))
;(visualize (graphic (cfg/build "while x < 10 do x := x + 1")))
;(visualize g)
