(ns z3.solver
  (:require [clojure.string :as s])
  (:use [clojure.java.shell :only [sh]]))

(def sat?
  (list 'check-sat))

(defn check-sat
  [model]
  ;(println (str (s/join (conj model sat?))))
  (-> (sh "z3" "-in" :in (str (s/join (conj model sat?))))
      :out
      s/trim
      (#(if (= % "sat")
          true
          false))))

(defn solve
  [model]
  (let [res (-> (sh "z3" "-in" :in (str (s/join (conj model
                                                      sat?
                                                      (list 'get-model)))))
                :out)
        vars (map read-string (re-seq #"(?<=define-fun\s)\w" res))
        values (map read-string (re-seq #"\d+" res))]
    (reduce conj {} (map vector vars values))))

(defmacro const
  "Coverts s-exp `(const name type)` to SMT format"
  [cname ctype]
  `(list ~'(symbol 'declare-const) ~cname '~ctype))

(defmacro assert
  "Converts s-exp `(assert var oper value)` to SMT format"
  [exp1 oper exp2]
  `(if (= (str ~oper) "!=")
     (list ~'(symbol 'assert) (list ~'(symbol 'not) (list ~'(symbol '=) ~exp1 ~exp2)))
     (list ~'(symbol 'assert) (list ~oper ~exp1 ~exp2))))

(defmacro not
  "Converts s-exp `(keyword comp-exp)` to `(keyword (not comp-exp)`"
  [exp]
  `(list (symbol (first ~exp)) (list ~'(symbol 'not) (second ~exp))))

; (def test_model (conj [] (list 'declare-const 'a 'Int) 
;                          (list 'assert (list '> 'a '10))))

; (def test_model2 (conj [] (const 'b 'Int) (assert 'b > 10) (assert 'b < 5)))
; (check-sat [])
