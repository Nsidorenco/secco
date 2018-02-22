(ns secco.interpreter-test
	(:require [clojure.test :refer :all]
			  [secco.interpreter :refer :all]
			  [secco.cfg :refer :all]
			  [clojure.java.io :as io]))

(defn beforetest [f]
	(reset! res 0)
	(reset! venv {})
	(f))

(use-fixtures :each beforetest)

(deftest parentest
	(interpret (build "3*(3+2)"))
	(testing "precedence"
		(is (= @res 15))))

(deftest branchingtest
	(interpret (build "if 3<4 then 1 else 2"))
	(testing "true_branch"
		(is (= @res 1)))
	(interpret (build "if 4<3 then 1 else 2"))
		(testing "false_branch"
			(is (= @res 2))))

(deftest looptest
	(interpret (build "(i:=0; while i<5 do i:=i+1; i)"))
	(testing "loop"
		(is (= @res 5))))

(deftest modtest
	(interpret (build (slurp (io/resource "test-programs/modtest.sec"))))
	(testing "res of modtest.sec"	
		(is (= @res 4)))
	(testing "venv of modtest.sec"
		(is (= @venv {"a" 112, "b" 9, "i" 4}))))

