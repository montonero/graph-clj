(ns graph.dijkstra
  (:require [loom.graph :refer :all] [loom.gen :refer :all]
            [loom.attr :refer :all] [loom.io :refer :all])
  (:gen-class))


(def rwg (gen-rand (weighted-digraph) 5 10 :max-weight 10))
(view rwg)
(do
  (println (edges rwg))
  (println (nodes rwg))
  )

(weight rwg 0 4)

(def es (edges rwg))

(defn nodes-from [g node]
  ; get list of nodes a given node is connected to
  (let [es (edges g)]
    (map second (filter #(= (first %) node) es)))
  )

(nodes-from rwg 3)

; map of weights
(defn node-weights-from [g node]
  (let [neighbours (nodes-from g node)]
    (zipmap neighbours (map #(weight g node %) neighbours))
  )
  )

(node-weights-from rwg 0)

(defn loom-digraph-to-map [g]
  (reduce #(assoc %1  %2 (node-weights-from g %2)) {} (nodes g))
  )

(loom-digraph-to-map rwg)
; {0 {2 1, 3 3}, 1 {4 8}, 4 {2 1}, 3 {1 7}, 2 {}}
(def my-g (loom-digraph-to-map rwg))
(count my-g)
my-g
( my-g 1)


(def v (vec (range 5)))

(def arr
  (to-array-2d(repeat 10
(into (vector) (repeat 10 0))
                     ))
  )

(def arr
  (to-array-2d(repeat 10
(into (vector) (range 10))
                     ))
  )

(aget arr 0 9)

(map seq (seq arr))

;;;;;;;;
(defn empty-map [m n val]
  (zipmap (range m n) (repeat val)))

(defn empty-map-keys [keys val]
  (zipmap keys (repeat val))
  )

Integer/MAX_VALUE

; todo fix prev
(defn my-dijkstra [graph src]
(let [ n (count graph),
      nodes (keys graph)]
  (loop [curr src
         unv (set (keys graph)), ; set of nodes that are not visited
         ;dist (assoc (empty-map-keys nodes Integer/MAX_VALUE) src 0),
         dist (assoc (empty-map-keys nodes Integer/MAX_VALUE) src 0),
         prev (empty-map-keys nodes nil)]
    (if (empty? unv)
      ; if no nodes are unvisited then stop
      [dist prev]
    (let [unvi (disj unv curr)
          nextu (first (sort-by #(dist %) unvi)) ; new unvisited node with minimal 'dist' value
          unv-edges (select-keys (graph curr) unvi) ; edges to unvisited nodes from a newly added node
          ]
      ;(if (empty? curr-neigh)
      (if (empty? unv-edges)
        (recur nextu unvi dist prev)
        (let [newd (reduce #(assoc %1 %2 (min (dist %2) (+ (dist curr) (unv-edges %2)))) {} (keys unv-edges))
              nextd (first (sort-by second newd))
              dist' (merge dist newd)
              prev' (assoc prev (first nextd) curr)
              ; recompute new unvisited as dist has changed
              nextu (first (sort-by #(dist' %) unvi))]
          (recur nextu unvi dist' prev')
          )
        )
      )
      )
    )
  )
  )

(first (my-dijkstra my-g 0))
(second (my-dijkstra my-g 0))
my-g

(def test-map {1 1, 2 100, 3 5, 4 3})
(def unvi (set (range 1 5)))
unvi
(sort-by #(test-map %) unvi)

