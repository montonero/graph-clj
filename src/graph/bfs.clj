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
    (loop [visited-nodes (assoc (empty-map (count g) -1) start-node 0),
           q (conj clojure.lang.PersistentQueue/EMPTY start-node),
           explored #{}]
      (if (empty? q)
        ; that's it
        visited-nodes
        ; visit current-node
        (let [current-node (peek q),
              q2 (pop q),
              neighbours (get g current-node),
              neigh-noexplored (remove explored neighbours),
              visited-new (reduce #(assoc %1 %2 (inc (visited-nodes current-node))) {} neigh-noexplored)]
          (recur (merge visited-nodes visited-new)  (into q2 neigh-noexplored) (into explored neighbours))
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

    (loop [visited-nodes (assoc (empty-map (count g) -1) start-node 0),
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
            (recur (merge visited-nodes (reduce #(assoc %1 %2 (inc (visited-nodes current-node))) {} neigh-noexplored)) (inc steps) (into q2 neigh-noexplored) (into explored neighbours))
            )
          )
        )
    )
)




(pos? 1)
my-g

(def mymap (empty-map (count my-g) -1))
mymap
(assoc mymap 3 2)
(def res (bfs my-g 1))
(def res (bfs (assoc my-g 7 []) 1))
res

(defn print-search-results [m]
  ; print results of bfs search (which is a map of nodes and times it takes to get to them)
  (map #(printf "%s " %) (keys m))
  )
(count res)
(keys res)
(clojure.string/join " " (map #(if (neg? %) % (* 6 %)) (vals (sort res))))


(map #(+ 1 %) (vals res))

(vals res)
(print-search-results res)

(re-seq #"\w+" "Hey you out there in the aisle")

(def mymap2 {0 1, 1 [2 3], 2 [4 5 6], -1 7})
mymap2
(clojure.set/map-invert {:a 1, :b 2})
(clojure.set/map-invert mymap2)

(defn invert-my [k vs]
  ; given key and values in vs creates a map
  ; where each value from vector is a key
  (reduce #(assoc %1 %2 k) {} vs)
  )

(invert-my 2 [4 5 6])

;(def gxx (slurp "data/graph1.txt"))

(def gxx  {65 #{7 58 27 24 55 46 54 15 48 32 36 41 43 61 29 51 17 3 12 66 35 5 45 53 38 10 52 8 49}, 70 #{58 60 1 55 39 46 48 50 21 32 13 36 44 64 34 17 66 11 9 45 38 42 37 49}, 62 #{65 70 7 59 24 55 32 36 61 44 34 12 35 68 5 14 45 53 26 16 10 52 67 37 63 49}, 7 #{62 20 58 27 69 24 55 4 15 48 31 32 40 36 28 64 12 35 19 57 11 9 30 37}, 59 #{20 58 60 27 24 55 40 13 41 61 28 17 2 23 9 26 16 38 30 52 67 42 63}, 20 #{7 27 54 15 56 13 41 28 64 51 25 34 3 12 47 19 57 9 14 53 18 49}, 58 #{7 60 39 54 21 31 33 41 61 44 28 51 34 66 5 26 30 10 63}, 60 #{7 59 20 27 1 69 39 46 4 54 21 31 32 40 33 41 43 61 6 28 51 2 47 68 5 8}, 27 #{7 20 60 1 46 54 50 31 32 40 29 44 28 64 34 66 11 5 14 30 37}, 1 #{70 7 27 69 55 46 4 15 48 21 32 40 56 33 13 22 41 43 44 28 64 34 17 3 12 2 47 19 9 45 53 16 38 10 52 67 63 8 49}, 69 #{62 58 27 4 54 15 21 31 32 40 56 22 29 6 35 19 11 45 26 16 30 10 18 52 63}, 24 #{70 7 58 60 1 69 4 15 48 32 36 44 6 51 25 66 47 57 11 5 45 16 30 10 18 52}, 55 #{59 24 46 15 32 56 33 36 41 29 6 28 3 66 47 35 19 68 9 5 53 30 18 67 37 49}, 39 #{70 62 59 20 58 69 24 15 40 22 29 12 23 11 26 38 67}, 46 #{65 70 58 60 27 69 39 50 32 33 13 22 36 29 6 28 51 47 68 9 5 14 16 38 30 10 67}, 4 #{65 59 27 46 48 21 32 56 33 22 61 44 28 51 47 35 11 5 10 37}, 54 #{70 62 59 60 1 55 39 15 31 32 56 33 22 36 29 28 64 34 19 57 9 14 45 38 30 52 67}, 15 #{1 69 55 39 50 31 32 41 43 61 44 17 2 47 19 57 68 16 30 63}, 48 #{70 7 59 27 1 39 4 54 21 31 32 33 13 22 29 44 6 64 34 66 57 11 9 5 14 53 26 38 49}, 50 #{65 62 59 58 27 24 55 46 4 48 56 36 61 29 51 25 17 12 66 19 9 16 38 10 8}, 21 #{62 20 58 27 1 55 50 31 32 40 56 33 22 41 43 29 28 25 34 66 35 19 57 68 10 42 37}, 31 #{62 7 59 20 58 1 69 4 15 21 56 51 34 3 19 11 5 26 10 18 52 67}, 32 #{7 20 27 1 55 4 15 56 22 36 6 34 12 2 35 19 68 18 52 37}, 40 #{65 70 62 20 27 69 50 56 41 44 6 25 68 11 5 53 38}, 56 #{7 20 46 32 33 13 29 28 66 47 35 68 11 14 18}, 33 #{27 1 69 4 54 48 31 13 44 51 25 3 2 66 57 68 9 45 30 63 49}, 13 #{65 70 7 59 20 69 39 21 25 17 3 2 23 35 9 14 38 30}, 22 #{70 20 27 1 55 48 50 13 41 43 25 34 2 23 47 5 45 38 30 10 52 42}, 36 #{65 70 62 59 20 58 27 1 39 4 54 48 50 21 31 13 12 66 23 57 45 67 37 49}, 41 #{65 70 27 1 4 54 48 50 32 56 33 43 44 6 17 19 26 16 67 63}, 43 #{65 62 7 59 60 54 21 32 40 61 44 64 51 25 35 26 30 52 37 49}, 61 #{58 1 55 54 50 31 32 41 28 64 34 23 19 11 9 14 26 16 38 67 49}, 29 #{65 60 27 69 24 40 44 64 17 11 9 5 45 53 26 67 42 37}, 44 #{65 59 27 1 69 54 15 48 21 31 32 56 41 28 51 25 34 3 66 68 9 26 30 52 67 8}, 6 #{65 27 39 4 40 33 61 64 34 3 2 57 14 16 37 8}, 28 #{65 58 1 69 24 39 48 50 21 56 13 36 12 11 14 53 16 42 8}, 64 #{65 62 59 58 60 4 54 15 48 32 44 28 17 12 35 11 14 63}, 51 #{62 7 58 1 39 54 15 13 41 43 25 17 12 66 57 11 9 53 16 38 30 18 52 67 49}, 25 #{70 58 60 39 31 40 41 64 17 66 47 35 5 53 10 18 63 8 49}, 34 #{70 62 59 58 60 1 69 24 39 15 31 56 13 29 6 28 64 51 3 12 66 23 57 45 30 10 63 49}, 17 #{58 69 24 46 54 50 21 31 32 40 56 22 36 28 66 19 11 9 14 26 30 18 67}, 3 #{65 70 59 60 27 69 24 46 4 50 21 31 33 13 6 25 2 66 47 19 57 9 14 45 16 38 49}, 12 #{62 7 55 48 21 31 22 61 44 6 64 51 34 2 23 47 19 68 11 5 26 16 10 18 52 37}, 2 #{65 62 1 69 15 48 50 31 56 33 22 36 43 61 29 28 25 3 66 23 47 35 68 11 5 45 26 16 30 10 63 8}, 66 #{62 58 55 15 21 31 22 36 43 61 44 6 34 3 23 47 68 11 9 5 45 53 26 10 18 42}, 23 #{7 59 1 69 46 56 33 43 34 19 68 9 5 45 53 18 37 63}, 47 #{15 48 21 40 13 41 61 29 44 28 64 25 34 2 66 35 45 53 16 38 30 18 52 42 37 63 49}, 35 #{1 55 46 48 32 13 43 29 34 23 47 57 68 5 14 45 26 38 10 67 63 49}, 19 #{65 70 7 1 24 55 46 15 48 22 44 28 64 12 35 68 45 16 38 67 42 37 8}, 57 #{65 70 7 58 60 1 39 54 15 48 50 31 40 13 61 29 28 66 23 19 14 53 16 10 18 37 63}, 68 #{7 1 69 24 55 4 48 50 21 22 28 51 25 3 35 11 53 26 38 30 52 67 8}, 11 #{70 60 1 39 4 31 40 13 22 36 29 51 34 17 3 23 35 68 5 26 16 30 37}, 9 #{62 7 20 1 24 4 48 31 32 40 22 61 6 64 17 47 35 19 45 10 18 52 63}, 5 #{70 62 59 24 15 21 13 29 44 64 66 68 10 18 37 63}, 14 #{70 62 7 59 58 69 46 4 54 15 40 28 51 25 68 9 45 26 38 30}, 45 #{65 7 60 1 69 46 54 33 22 43 6 34 57 11 5 30 18 67}, 53 #{24 39 21 13 43 28 64 51 25 23 47 68 11 9 26 38 10 63 8}, 26 #{59 20 1 69 55 4 54 15 22 36 51 25 34 3 23 14 53 16 10 18 8 49}, 16 #{70 20 58 1 69 39 46 4 21 33 13 22 36 43 28 3 2 57 14 49}, 38 #{62 27 1 55 39 46 31 56 13 22 61 29 28 64 17 3 47 57 11 9 14 52 67}, 30 #{65 62 59 27 1 46 54 48 32 41 61 6 51 3 23 57 45 26 8 49}, 10 #{65 7 58 60 31 40 43 29 28 64 51 57 11 5 53 30 18 52 42}, 18 #{55 31 32 33 22 44 25 12 66 45 10 52 67 42 63 8}, 52 #{62 59 58 60 27 1 24 39 21 31 33 44 34 2 57 9 45 26 16 38 18 42 8}, 67 #{62 58 27 1 24 15 31 32 22 41 43 61 44 64 51 25 57 68 53 30 10 18 52}, 42 #{65 62 20 55 39 56 22 36 43 44 51 25 12 66 47 35 19 57 14 26 38 10 63}, 37 #{62 59 20 58 60 1 69 54 40 41 61 29 6 64 12 47 14 45 38 42}, 63 #{70 7 20 69 55 39 4 48 22 43 29 6 64 3 26 18 52 49}, 8 #{65 70 27 1 24 55 39 46 31 32 40 36 41 51 17 19 57 68 5 14 45 16 37 63}, 49 #{62 59 20 27 1 69 24 46 4 48 50 31 32 40 43 12 19 57 14 53 26 16 18 63}}
  )

(sort (bfs gxx 16))
((bfs gxx 16) 5)

(with-open [rdr (clojure.java.io/reader "data/graph1.txt")]
         (count (line-seq rdr)))
