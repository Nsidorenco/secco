(ns secco.util
  )

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
(def literals (map (fn [x] (read-string (clojure.string/join x))) (subsets 3 alphabet)))

(defn pop-head!
  [a]
  (-> (swap! a #(with-meta (subvec % 1) {:head (% 0)}))
      (meta)
      :head))
