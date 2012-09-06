(ns life-live.core)

(def deltas [-1 0 1])

(def world (atom #{[1 2] [1 1] [1 3] [2 3] [3 2]}))

(defn neighbours 
  "Calcule les voisins de la cellule." 
  [[x y]]
  (for [dx deltas, dy deltas
        :when (not= dx dy 0)]
    [(+ dx x) (+ dy y)]))

(defn stepper [voisins]
  (fn [cells] 
    (let [freqs (frequencies 
                  (mapcat voisins cells))]
      (set (for [[cell n] freqs
                 :when (or (= n 3)
                           (and (= n 2)
                                (cells cell)))]
             cell)))))

(def step (stepper neighbours))

(defn rect-neighbours
  [[x y]]
  (for [dx deltas, dy deltas
        :when (not= dx dy 0)
        :let [x (+ dx x)
              y (+ dy y)]
        :when (and (<= 0 x 10)
                   (<= 0 y 10))]
    [x y]))

(def rect-step
  (stepper rect-neighbours))





