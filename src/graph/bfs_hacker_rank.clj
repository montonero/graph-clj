;https://www.hackerrank.com/challenges/bfsshortreach
(ns graph.bfs-hacker-rank
  (:require [loom.graph :refer :all] [loom.gen :refer :all]
            [loom.attr :refer :all] [loom.io :refer :all])
  (:gen-class))



;(def num-tests (read))


(defn empty-map [n val]
  ; create an empty map with keys in range [1..n] with equal values 'val'
  (zipmap (range 1 (inc n)) (repeat val)))

(defn add-edge [g x y]
  ; add edge (x, y) to graph g
  (println "Adding edge: " x y)
   (update (update g x #(conj (set %) y)) y #(conj (set %) x))
  )



(defn read-graph [num-vert num-edges]
  ; read 'num-edges' lines containing edges in 'x y' format
  (loop [g (empty-map num-vert []),
         n num-edges]
    (if (pos? n)
      (let [a (read) b (read) ]
        (recur (add-edge g a b) (dec n) ))
      (do
        (println "Read graph: " g)
        ;(view (graph g))
        g)
      )
    )
  )

(defn bfs [g start-node]
  ; visit-nodes - is map associating node with steps it takes to get to them from start-node
  ;   if node is unreachable then value is -1
  ; q - queue of nodes to visit
  ; explored - set of already explored or added to the queue nodes
  (loop [visited-nodes (assoc (empty-map (count g) -1) start-node 0),
         q (conj clojure.lang.PersistentQueue/EMPTY start-node),
         explored #{}]
    (if (empty? q)
      visited-nodes
      (let [current-node (peek q),
            neighbours (get g current-node),
            neigh-noexplored (remove explored neighbours),
            visited-new (reduce #(assoc %1 %2 (inc (visited-nodes current-node))) {} neigh-noexplored)]
        ; we construct a 'visited-new' map from (current node's neighbours minus already explored nodes
        (recur (merge visited-nodes visited-new)  (into (pop q) neigh-noexplored) (into explored neigh-noexplored)))
      )
    )
)

(defn do-test-case []
  (let [num-vert (read), num-edges (read),
        g (read-graph num-vert num-edges),
        start-node (read),
        res (bfs g start-node)]
    ; print distance of all vertices except the start-node
    ;(println (clojure.string/join " " (map #(if (neg? %) % (* 6 %)) (vals (sort (dissoc res start-node))))))
    (println (sort (g start-node)))
    (println (sort res))
    )
  )



