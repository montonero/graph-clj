(ns graph.dijkstra
  (:require [loom.graph :refer :all] [loom.gen :refer :all]
            [loom.attr :refer :all] [loom.io :refer :all])
  (:gen-class))


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
(defn empty-map [n val]
  (zipmap (range 1 (inc n)) (repeat val)))

Integer/MAX_VALUE

Long/MAX_VALUE

(let [n 5, dist (assoc (empty-map n Integer/MAX_VALUE) 1 0),
      prev (empty-map n -1),
      q (set (range 1 n)),
      u (second (first (sort-by first (map #(list (dist %) %) q))))
      u2 (sort-by #(dist %) q)]
  (println q dist prev)
  (println u u2)
  (sort-by first (map #(list (dist %) %) q))
  )


(def test-map {1 1, 2 100, 3 5, 4 3})
(def unvi (set (range 1 5)))
unvi
(sort-by #(test-map %) unvi)
