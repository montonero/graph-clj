(ns graph.bfs
  (:require [loom.graph :refer :all] [loom.gen :refer :all]
            [loom.attr :refer :all] [loom.io :refer :all])
  (:gen-class))



(def a {1 [2 3] 2 [3] 3 []})

(view (graph a))
(view (digraph a))




(defn empty-graph [n]
  (zipmap (range 1 (inc n)) (repeat [])))

(def my-g (merge (empty-graph 6) a))

my-g

(get my-g 1)

(defn add-edge [g x y]
  ; add edge (x, y) to graph g
  (update g x #(conj % y))
  )

(def g02 (add-edge (add-edge my-g 1 2) 1 3))

(view (digraph g02))
(view (digraph my-g))



(def q (conj (conj clojure.lang.PersistentQueue/EMPTY 1) 2))
(def q (into clojure.lang.PersistentQueue/EMPTY (get my-g 1)))
;(def q2 (conj clojure.lang.PersistentQueue/EMPTY
(peek q)
(def q2 (pop q))
(peek q2)
(empty? (pop q2))
(empty? q)

(defn dummy [queue]
  (loop [qq queue]
    (if (empty? qq)
      nil
      (do
        (println (peek qq))
        (recur (pop qq))
        )
      )
    )
)

(dummy q)


(defn bfs [g start-node]
  (let [frontier (get g start-node)]
    (loop [q (into clojure.lang.PersistentQueue/EMPTY frontier),
           explored #{}]
      (let [current-node (peek frontier),


