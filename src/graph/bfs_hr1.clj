(use '[clojure.string :only (split trim)])

(def read-int #(Integer/parseInt (trim (read-line))))
(defn read-int-seq []
    (map #(Integer/parseInt %) (split (trim (read-line)) #"\s+"))
)

(defn adjacency-list [edges]
    (let [
        edges-both (reduce #(into %1 (set [%2 (vec (reverse %2))])) #{} edges)
        adj-map (group-by first edges-both)
        adj (reduce-kv #(assoc %1 %2 (map second %3)) {} adj-map)
        ]
        adj
    )
)

(defn bfs [n adj s]
    (loop [
        queue [s]
        distance {s 0}
        cur-dist 1
        ]
        (if (empty? queue)
            distance
            (let [
                queue-adj (reduce into #{} (map #(adj %) queue))
                next-queue (filter #(not (contains? distance %)) queue-adj)
                next-distance (reduce #(merge %1 {%2 cur-dist}) distance next-queue)
            ]
                (recur next-queue next-distance (inc cur-dist))
            )
        )
    )
)

(defn get-ans [n s distance]
    (map #(if (contains? distance %)
        (* 6 (distance %))
        -1)
    (filter #(not= % s) (range 1 (inc n))))
)

(defn solve []
    (let [
        [n m] (read-int-seq)
        edges (repeatedly m #(vec (read-int-seq)))
        adj (adjacency-list edges)
        s (read-int)
        distance (bfs n adj s)
        ans (get-ans n s distance)
    ]
        (apply println ans)
    )
)

(let [t (read-int)]
    (dotimes [it t] (solve))
)
