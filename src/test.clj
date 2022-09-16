(ns test
  (:require
   [clojure.test :refer [deftest is run-all-tests]]
   [stack :refer [defstackfn]]))

(deftest idenity>
  (defstackfn f [!a !b] !a !b)
  (is (= (f 1 2) 2)))

(deftest <pop>
  (defstackfn f [!a !b] !a !b <pop>)
  (defstackfn g [!a !b] <pop> <pop> !a <pop> !b)
  (is (= (f 1 2) 1))
  (is (= (g 10 20) 20)))

(deftest variables>
  (defstackfn f [] 1 !v+ <pop> !v)
  (defstackfn g [!a] 3 !a+ <pop> !a)
  (is (= (f) 1))
  (is (= (g 1) 3)))

(deftest invoke>
  (defstackfn f [!a !b !c] !c !b (invoke> - 2) !a (invoke> + 2))
  (is (= (f 1 2 3) 2))
  (is (= (f 10 20 30) 20))
  (is (= (f 300 20 1) 281)))

(deftest if>else>
  (defstackfn f [!a !b !c] !c (if> !b !a (invoke> + 2) else> <pop> !a !b (invoke> * 2)))
  (defstackfn g [!a !b !c] (if> !b !a (invoke> + 2) else> <pop> !a !b !c (invoke> * 2)))
  (is (= (f 1 2 3)  3))
  (is (= (f 10 2 false) 20))
  (is (= (f 10 2 3) 12))
  (is (= (g 1 2 3) 6))
  (is (= (g 9 6 3) 18)))

(deftest misc>
  (defstackfn f [!a !b !c !2]
    !a !vv+ 12 !a+ !b (invoke> + 2) !vv !c "thing" <pop> !2 (invoke> - 2) !a (invoke> + 5)
    (if> !a !b else> !c !a) (invoke> * 3))
  (is (= (f 1 2 3 4) 648))
  (is (= (f 10 20 30 40) 12960)))

(run-all-tests)
