(ns secco.viz
  (:require [clojure.core.match :refer [match]]
            [secco.cfg :as cfg]
            [secco.util :as util]))

(try
  (require '[rhizome.viz :as r])
  (catch Exception e
    (println "Needs rhizome.viz")))

(def labels (atom {}))
(def edges (atom {}))

(defn visualize [graph] (r/view-graph (keys graph) graph
                                      :node->descriptor (fn [n] {:label (get @labels n)})
                                      :edge->descriptor (fn [src dst] {:label (dst (src @edges))})))

(def graph (atom {}))

(defn pretty [body]
  (match [(first body)]
         [:VarExp] (pretty (second body))
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
         [:SimpleVar] (second body)
         [:ArrayVar] (str (second body) "[" (pretty (second (last body))) "]")
         [:Array] (str "array(" (pretty (second body)) ")")
         [:UserInput] "input()"
         [:Size] (str "size(" (pretty (second body)) ")")
         [:Error] "Error"
         [_] body))

(defn unfold [exp]
  (match [(first exp)]
    [:add] (let [[_ exp1 exp2] exp]
              (str (pretty (unfold exp1)) "+" (pretty (unfold exp2))))
    [:sub] (let [[_ exp1 exp2] exp]
              (str (pretty (unfold exp1)) "-" (pretty (unfold exp2))))
    [:mul] (let [[_ exp1 exp2] exp]
              (str (pretty (unfold exp1)) "*" (pretty (unfold exp2))))
    :else exp))

(defn build-tree 
  ([node] (build-tree node 0 {}))
  ([node counter mark]
   (if (instance? secco.cfg.CFGNode node)
     (let [
           getsym (nth util/symbols counter)]
       (match [(first (.exp node))]
              [nil] (let [ex (read-string (.nam node))
                             tnode (build-tree (cfg/get-t node) (+ counter 1) mark)]
                         (swap! labels conj {getsym "s_0"})
                         (swap! graph conj {getsym [tnode]})
                         getsym)
              [:OpExp] (let [
                             [_ exp1 oper exp2] (.exp node)
                             ex (str (pretty (unfold exp1)) " " (second oper) " " (pretty (unfold exp2)))
                             tnode (when (not (contains? mark node)) 
                                     (build-tree (cfg/get-t node) (+ counter 1) (conj mark {node getsym})))
                             fnode (if (not (contains? mark node)) 
                                     (build-tree (cfg/get-f node) (+ counter 256) (conj mark {node getsym}))
                                     :end)]

                            (if (not(contains? mark node))
                                (let [] (swap! labels conj {getsym ex})
                                  (swap! edges conj {getsym {tnode "true" fnode "false"}})
                                  (swap! graph conj {getsym [tnode fnode]})
                                  getsym)
                                (get mark node)))
              [:add] (let [[_ exp1 exp2] (.exp node) 
                           ex (str (pretty (unfold exp1)) "+" (pretty (unfold exp2)))
                           tnode (build-tree (cfg/get-t node) (+ counter 1) mark)]
                       (swap! labels conj {getsym ex})
                       (swap! graph conj {getsym [tnode]})
                       getsym)
              [:sub] (let [[_ exp1 exp2] (.exp node) 
                           ex (str (pretty (unfold exp1)) "-" (pretty (unfold exp2)))
                           tnode (build-tree (cfg/get-t node) (+ counter 1) mark)]
                       (swap! labels conj {getsym ex})
                       (swap! graph conj {getsym [tnode]})
                       getsym)
              [:mul] (let [[_ exp1 exp2] (.exp node) 
                           ex (str (pretty (unfold exp1)) "*" (pretty (unfold exp2)))
                           tnode (build-tree (cfg/get-t node) (+ counter 1) mark)]
                       (swap! labels conj {getsym ex})
                       (swap! graph conj {getsym [tnode]})
                       getsym)
              [:INT]  (let [
                             ex (second (.exp node))]

                         (swap! labels conj {getsym ex})
                         (swap! graph conj {getsym [(build-tree (cfg/get-t node) (+ counter 1) mark)]}) 
                         getsym)
              [:Error]  (let []
                         (swap! labels conj {getsym "error()"})
                         (swap! graph conj {getsym [(build-tree (cfg/get-t node) (+ counter 1) mark)]})
                         getsym)
              [:Size]  (let [[_ arr] (.exp node)]
                         (swap! labels conj {getsym (str "size(" (pretty arr) ")")})
                         (swap! graph conj {getsym [(build-tree (cfg/get-t node) (+ counter 1) mark)]})
                         getsym)
              [:VarExp] (let [
                               ex (pretty (second (.exp node)))]

                           (swap! labels conj {getsym ex})
                           (swap! graph conj {getsym [(build-tree (cfg/get-t node) (+ counter 1) mark)]})
                           getsym)
              [:AssignExp] (let [[_ varexp body] (.exp node)
                                  ex (str (pretty varexp) ":=" (pretty body))
                                  og (.exp node)]
                              (swap! labels conj {getsym ex})
                              (swap! graph conj {getsym [(build-tree (cfg/get-t node) (+ counter 1) mark)]})
                              getsym)))
     :end)))

(defn graphic [node]
  (reset! graph {:end []})
  (reset! labels {:end "s_n"})
  (reset! edges {:end {}})
  (build-tree node)
  @graph)

;(println @graph)
;(println @labels)
;(println @edges)
;(visualize (graphic (cfg/build "x:=42;a:=2;x+a")))
;(visualize (graphic (cfg/build "(x:=(2+3);x)")))
;(visualize (graphic (cfg/build "(x := 0;y := 1; if x < y then if y < 2 then 0 else 1 else x)")))
;(visualize (graphic (cfg/build "while x < 10 do x := x + 1")))
;(visualize (graphic (cfg/build "if 1+2+3*4-5+6>2 then 2 else 3")))
