(ns graph.dijkstra
  (:require [loom.graph :refer :all] [loom.gen :refer :all]
            [loom.attr :refer :all] [loom.io :refer :all])
  (:gen-class))


(def rwg (gen-rand (weighted-digraph) 1000 2000 :max-weight 100))
(view rwg)

(do
  (println (edges rwg))
  (println (nodes rwg))
  )

(weight rwg 0 4)

(def es (edges rwg))


(loom-digraph-to-map rwg)
; {0 {2 1, 3 3}, 1 {4 8}, 4 {2 1}, 3 {1 7}, 2 {}}
(def my-g (loom-digraph-to-map rwg))
(count my-g)
my-g
(view weighted-digraph my-g)


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
(defn my-dijkstra2 [graph src]
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
        (let [ ; new distances map for neighbours of a current node
               newd (reduce #(assoc %1 %2 (min (dist %2) (+ (dist curr) (unv-edges %2)))) {} (keys unv-edges))
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

(Math/log 100)
(Math/log 19)
(Math/log 2)

(defn my-dijkstra [graph src]
(let [ n (count graph),
      nodes (keys graph)]
  (loop [curr src
         unv (set (keys graph)), ; set of nodes that are not visited
         ;dist (assoc (empty-map-keys nodes Integer/MAX_VALUE) src 0),
         dist (assoc (empty-map-keys nodes Integer/MAX_VALUE) src 0),
         ]
    (if (empty? unv)
      ; if no nodes are unvisited then stop
      dist
    (let [unvi (disj unv curr)
          nextu (first (sort-by #(dist %) unvi)) ; new unvisited node with minimal 'dist' value
          unv-edges (select-keys (graph curr) unvi) ; edges to unvisited nodes from a newly added node
          ]
      ;(if (empty? curr-neigh)
      (if (empty? unv-edges)
        (recur nextu unvi dist )
        (let [ ; new distances map for neighbours of a current node
               newd (reduce #(assoc %1 %2 (min (dist %2) (+ (dist curr) (unv-edges %2)))) {} (keys unv-edges))
              nextd (first (sort-by second newd))
              dist' (merge dist newd)
              ; recompute new unvisited as dist has changed
              nextu (first (sort-by #(dist' %) unvi))]
          (recur nextu unvi dist' )
          )
        )
      )
      )
    )
  )
  )


(def my-g2 {1 {2 24, 4 20}, 2 {}, 3 {1 3}, 4 {3 12}})
(def my-g2 {7 {1 2, 3 10, 6 42, 14 54, 13 66, 16 21, 19 52, 9 21, 12 13}, 20 {14 14}, 1 {7 2, 4 48, 15 34, 17 20, 3 5, 12 46, 2 28, 9 28, 5 66, 10 33, 18 46}, 4 {1 48}, 15 {14 19, 1 34}, 13 {7 66}, 6 {7 42, 14 26}, 17 {1 20}, 3 {7 10, 1 5, 14 27}, 12 {14 19, 1 46, 7 13}, 2 {14 15, 1 28}, 19 {7 52}, 11 {14 64}, 9 {1 28, 14 34, 7 21}, 5 {1 66}, 14 {7 54, 20 14, 15 19, 6 26, 3 27, 12 19, 2 15, 11 64, 9 34, 8 43}, 16 {7 21}, 10 {1 33}, 18 {1 46}, 8 {14 43}})

(vals (sort (my-dijkstra my-g2 17)))

(view (weighted-graph my-g2))

(cost-to-hr (first (my-dijkstra my-g 0)))
(second (my-dijkstra my-g 0))
my-g

(def test-map {1 1, 2 100, 3 5, 4 3})
(def unvi (set (range 1 5)))
unvi
(sort-by #(test-map %) unvi)


;;
(defn empty-map [n val]
  ; create an empty map with keys in range [1..n] with equal values 'val'
  (zipmap (range 1 (inc n)) (repeat val)))

;;

(defn add-edge-nr [g x y r]
  (let [old-val (get-in g [x y] Integer/MAX_VALUE)
        min-val (min old-val r)]
    (assoc-in g [x y] r)
    )
  )

my-g2

(view (weighted-digraph (add-edge my-g2 2 3 99)))



(add-edge my-g2 2 3 88)

(defn add-edge [g x y r]
  ; add edge (x, y) to graph g
  (add-edge-nr (add-edge-nr g x y r) y x r)
  )


;; read graph from command line
(defn read-graph [num-vert num-edges]
  ; read 'num-edges' lines containing edges in 'x y' format
  (loop [g (empty-map num-vert {}),
         n num-edges]
    (if (pos? n)
      (let [x (read) y (read) r (read)]
        (recur (add-edge g x y r) (dec n) ))
        g
      )
    )
  )

; replace unreachable nodes' value with -1
(defn cost-to-hr [m]
  (map #(if (= % Integer/MAX_VALUE) -1 %) (vals m))
  )


(defn do-test-case []
  (let [num-vert (read), num-edges (read),
        g (read-graph num-vert num-edges),
        start-node (read),
        res (first (my-dijkstra g start-node))]
    (println (clojure.string/join " " (cost-to-hr (dissoc res start-node))))
    ;(println (sort (g start-node)))
    ;(println (sort res))
    )
  )




(pos? 1)




(def ^:private inf Double/POSITIVE_INFINITY)

(defn update-costs
  "Returns costs updated with any shorter paths found to curr's unvisisted
  neighbors by using curr's shortest path"
  [g costs unvisited curr]
  (let [curr-cost (get costs curr)]
    (reduce-kv
      (fn [c nbr nbr-cost]
        (if (unvisited nbr)
          (update-in c [nbr] min (+ curr-cost nbr-cost))
          c))
      costs
      (get g curr))))

(defn dijkstra
  "Returns a map of nodes to minimum cost from src using Dijkstra algorithm.
  Graph is a map of nodes to map of neighboring nodes and associated cost.
  Optionally, specify destination node to return once cost is known"
  ([g src]
    (dijkstra g src nil))
  ([g src dst]
    (loop [costs (assoc (zipmap (keys g) (repeat inf)) src 0)
           curr src
           unvisited (disj (apply hash-set (keys g)) src)]
      (cond
       (= curr dst)
       (select-keys costs [dst])

       (or (empty? unvisited) (= inf (get costs curr)))
       costs

       :else
       (let [next-costs (update-costs g costs unvisited curr)
             next-node (apply min-key next-costs unvisited)]
         (recur next-costs next-node (disj unvisited next-node)))))))




(
  (time (dijkstra my-g 17))
  (time (my-dijkstra my-g 17))
  )

























