(ns graph.bfs
  (:require [loom.graph :refer :all] [loom.gen :refer :all]
            [loom.attr :refer :all] [loom.io :refer :all])
  (:gen-class))



(def a {1 [2 3] 2 [3] 3 [4 5]})

;(view (graph a))
;(view (digraph a))


(defn empty-map [n val]
  (zipmap (range 1 (inc n)) (repeat val)))

(def my-g (merge (empty-map 6 []) a))
(get my-g 1)
my-g

(defn add-edge [g x y]
  ; add edge (x, y) to graph g
  (update g x #(conj (set %) y))
  )

(def my-g (add-edge (add-edge my-g 5 6) 1 3))
(add-edge my-g 3 4)
g02
(view (digraph g02))
my-g
(view (digraph my-g))

(vec (set [1 2 3]))
(conj (set [1 2 3]) 5)


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

(def xxx (into (into clojure.lang.PersistentQueue/EMPTY [1 2]) '[a b]))
(seq xxx)
(peek (pop (pop xxx)))


(defn bfs [g start-node]
  (let [frontier (get g start-node)]
    (loop [q (into clojure.lang.PersistentQueue/EMPTY frontier),
           explored #{}]
      (let [current-node (peek q),
            q2 (pop q)]
        (if (contains? explored current-node)
          (if (empty? q2)
            ; that's it
            nil
            (recur q2 explored)
            )
          ; visit current-node
          (do
            (println "Visiting: " current-node)
            (recur (into q2 (get g current-node)) (conj explored current-node))
            )
          )
        )
      )
    )
  )

(defn bfs2 [g start-node]
  (let [frontier (get g start-node)]
    (loop [visited-nodes [],
           steps 1,
           q (into clojure.lang.PersistentQueue/EMPTY frontier),
           explored #{}]
      (let [current-node (peek q),
            q2 (pop q)]
        (if (contains? explored current-node)
          (if (empty? q2)
            ; that's it
            nil
            (recur visited-nodes steps q2 explored)
            )
          ; visit current-node
          (do
            (println "Visiting: " current-node "Step:" steps)
            (recur (conj visited-nodes current-node) (inc steps) (into q2 (get g current-node)) (conj explored current-node))
            )
          )
        )
      )
    )
  )

(count my-g)

(defn bfs3 [g start-node]

    (loop [visited-nodes (empty-map (count g) -1),
           steps 1,
           q (conj clojure.lang.PersistentQueue/EMPTY start-node),
           explored #{}]
      (if (empty? q)
        ; that's it
        visited-nodes
        ; visit current-node
        (let [current-node (peek q),
              q2 (pop q),
              neighbours (get g current-node),
              neigh-noexplored (remove explored neighbours)]
          (do
            (println "Visiting: " current-node "Step:" steps)
            (println "Queue: " (seq q) "Explored: " explored)
            (println "removed: " (remove explored (get g current-node))  "non removed: " (get g current-node))
            (recur (assoc visited-nodes current-node steps) (inc steps) (into q2 neigh-noexplored) (into explored neighbours))
            )
          )
        )
    )
)

my-g

(def mymap (empty-map (count my-g) -1))
mymap
(assoc mymap 3 2)
(bfs3 my-g 1)

(def mymap2 {0 1, 1 [2 3], 2 [4 5 6], -1 7})
mymap2
(clojure.set/map-invert {:a 1, :b 2})
(clojure.set/map-invert mymap2)


