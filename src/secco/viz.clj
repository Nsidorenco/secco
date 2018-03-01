(ns secco.viz
  (:require [clojure.core.match :refer [match]]
            [secco.cfg :as cfg]))

(try
  (require '[rhizome.viz :as r])
  (catch Exception e
    (println "Needs rhizome.viz")))

(def alphabet
  (map char (range 97 123)))

(defn subsets [n items]
  (cond
    (zero? n) '(())
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
                                      :edge->descriptor (fn [src dst] {:label (dst (src @edges))})))

(def graph (atom {}))

(defn pretty [body]
  (match [(first body)]
         [:VarExp] (second body)
         [:INT] (second body)
         [:OPER] (second body)
         [:add] (let [[_ exp1 exp2] body]
                    (str (pretty exp1) "+" (pretty exp2)))
         [:sub] (let [[_ exp1 exp2] body]
                    (str (pretty exp1) "-" (pretty exp2)))
         [:mul] (let [[_ exp1 exp2] body]
                    (str (pretty exp1) "*" (pretty exp2)))
         [:OpExp] (let [[_ exp1 oper exp2] body]
                    (str (pretty exp1) (pretty oper) (pretty exp2)))
         [:ParenExp] (str "(" (pretty (second body)) ")")
         [_] body))

(defn unfold [exp]
  (match [(first exp)]
    [:add] (let [[_ exp1 exp2] exp]
              (str (unfold exp1) "+" (unfold exp2)))
    [:sub] (let [[_ exp1 exp2] exp]
              (str (unfold exp1) "-" (unfold exp2)))
    [:mul] (let [[_ exp1 exp2] exp]
              (str (unfold exp1) "*" (unfold exp2)))
    :else (second exp)))

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
                             ex (str (unfold exp1) (second oper) (unfold exp2))
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
              ["arith"] (let [type (first (.exp node))]
                            (let [
                                  oper (str (match [type]
                                              [:add] "+"
                                              [:sub] "-"
                                              [:mul] "*"))
                                  [_ exp1 exp2] (.exp node)
                                  ex (str (unfold exp1) oper (unfold exp2))
                                  tnode (build-tree (cfg/get-t node) (+ counter 1) mark)
                                ]
                             (swap! labels conj {getsym ex})
                             (swap! graph conj {getsym [tnode]}))
                             getsym)
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
                      (get mark node)))
         ["arith"] (let [type (first (.exp node))]
                     (let [oper (str (match [type]
                                       [:add] "+"
                                       [:sub] "-"
                                       [:mul] "*"))
                           [_ exp1 exp2] (.exp node)
                           ex (str (find exp1) oper (find exp2))
                           tnode (build-tree (cfg/get-t node) (+ counter 1) mark)]
                       (swap! labels conj {getsym ex})
                       (swap! graph conj {getsym [tnode]}))
                     getsym)
         ["int"]  (let [ex (second (.exp node))]
                    (swap! labels conj {getsym ex})
                    (swap! graph conj {getsym [(build-tree (cfg/get-t node) (+ counter 1) mark)]})
                    getsym)
         ["varexp"] (let [ex (second (.exp node))]
                      (swap! labels conj {getsym ex})
                      (swap! graph conj {getsym [(build-tree (cfg/get-t node) (+ counter 1) mark)]})
                      getsym)
         ["assignexp"] (let [[_ varexp body] (.exp node)
                             ex (str (second varexp) ":=" (pretty body))
                             og (.exp node)]
                         (swap! labels conj {getsym ex})
                         (swap! graph conj {getsym [(build-tree (cfg/get-t node) (+ counter 1) mark)]})
                         getsym)))
     :end)))

(defn graphic [node]
  (reset! graph {:end []})
  (reset! labels {:end "end"})
  (reset! edges {:end {}})
  (build-tree node)
  @graph)

; TODO: stop uending loops
;(println @graph)
;(println @labels)
;(println @edges)
;(visualize (graphic (cfg/build "x:=42")))
;(visualize (graphic (cfg/build "(x:=(2+3);x)")))
;(visualize (graphic (cfg/build "(x := 0;y := 1; if x < y then if y < 2 then 0 else 1 else x)")))
;(visualize (graphic (cfg/build "while x < 10 do x := x + 1")))
;(visualize g)
;(visualize (graphic (cfg/build "if 1+2+3*4-5+6>2 then 2 else 3")))
