(ns graph.even-tree
  (:require [loom.graph :refer :all] [loom.gen :refer :all]
            [loom.attr :refer :all] [loom.io :refer :all])
  (:gen-class))



;; Initialize with any of: edges, adacency lists, nodes, other graphs
(def g (graph [1 2] [2 3] {3 [4] 5 [6 7]} 7 8 9))
(def dg (digraph g))
(def wg (weighted-graph {:a {:b 10 :c 20} :c {:d 30} :e {:b 5 :d 5}}))
(def wdg (weighted-digraph [:a :b 10] [:a :c 20] [:c :d 30] [:d :b 10]))
(def rwg (gen-rand (weighted-graph) 10 20 :max-weight 100))
(def fg (fly-graph :successors range :weight (constantly 77)))


(view wg)

(def a (assoc {} 1 [2]))
a
(def a {1 [2 3] 2 [3] 3 []})
a

(view (graph a))
(view (digraph a))

(class a)

(cons 3 [1])

(assoc a 1 [3])
a
(update a 1 (fn [x] (conj x 5)))

(def my-g (zipmap (range 1 6) (repeat [])))

(defn add-edge [g x y]
  ; add edge (x, y) to graph g
  (update g x #(conj % y))
  )

(add-edge (add-edge my-g 1 2) 1 3)


(defn print-down-from [x]
  (when (pos? x)
    (println x)
    (recur (dec x)))
  )


(defn print-down-from2 [x]
  (if (pos? x)
    (do (println x) (recur (dec x)))
    (println "Hello")
    )
  )

(print-down-from 10)
(print-down-from2 1)

(def ds (into-array [:willie :barnabas :adam]))

(class (seq ds))

(def q (conj (conj clojure.lang.PersistentQueue/EMPTY 1) 2))

(println (peek q))

(do
(println (peek q))
(println (peek (pop q)))
  )


(into [] '(a b c))

['a 'b 'c]

(def xyz '(x y z))

(= 'a :a)

(= :a :a)
[:a :b :c]

(symbol? 'a)
(keyword? :a)

(def a 'a)

(symbol? a)

