(ns hackerrank.algorithms.graphtheory.dijkstra
  (:require [clojure.test :refer :all] [clojure.string :as str] ))

(defn get-numbers [line]
  (mapv #(Integer/parseInt %) (str/split (str/trim line) #"\s+")))

(defn new-graph [n]
  (reduce (fn [m i] (assoc m i {})) {} (range 1 (inc n))))

(defn add-edge [gr [a b w]]
  (let [oldw (get-in gr [a b])]
    (if (or (nil? oldw) (> oldw w))
      (assoc gr
         a (assoc (get gr a) b w)
         b (assoc (get gr b) a w))
      gr)))

(defn get-graph [n edges]
  (reduce add-edge (new-graph n) edges))

(defn new-distances [n s]
  (assoc (zipmap (range 1 (inc n)) (repeat n -1)) s 0))

(defn get-distances [gr n s]
  (let [dis (atom (new-distances n s))]
    (loop [openv [s]]
      (if (empty? openv)
        @dis
        (let [a (first openv)
              len (get @dis a)
              neighbors (get gr a)
              newopenv (reduce (fn [newo [node weight]]
                                 (let [oldlen (get @dis node)
                                       newlen (+ len weight)]
                                (if (or (= oldlen -1) (> oldlen newlen))
                                  (do
                                    (swap! dis assoc node newlen)
                                    (conj newo node))
                                  newo)))
                              openv
                              neighbors)]
          (recur (subvec newopenv 1)))))))


(defn process []
  (let [[n m] (get-numbers (read-line))
        edges (doall (repeatedly m #(get-numbers (read-line))))
        s (Integer/parseInt (str/trim (read-line)))
        gr (get-graph n edges)
        ]
  (println  (str/join " " (vals (into (sorted-map) (dissoc (get-distances gr n s) s)))))
  ))


  (let [t (Integer/parseInt (read-line))]
    (dorun (repeatedly t process)))
