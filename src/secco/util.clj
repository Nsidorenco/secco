(ns secco.util
  (:require [secco.cfg :as cfg]))

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
(def literals (map (fn [x] (read-string (clojure.string/join x))) (subsets 3 alphabet)))

(defn pop-head!
  [a]
  (-> (swap! a #(with-meta (subvec % 1) {:head (% 0)}))
      (meta)
      :head))

(defn findSym
  ([node] (findSym node [] #{}))
  ([node visited acc]
   (if (cfg/node? node)
     (let [exp (.exp node)]
       (if (and (= (first exp) :AssignExp) (= (first (last exp)) :UserInput))
         (recur (cfg/get-t node) (conj visited node) (conj acc (-> exp
                                                                   second
                                                                   second
                                                                   second
                                                                   (str (second (last (second (second exp)))))
                                                                   read-string)))
         (when-not (some (partial identical? node) visited)
           (findSym (cfg/get-t node) (conj visited node) acc)
           (recur (cfg/get-f node) (conj visited node) acc))))
     acc)))