(ns life-live.tron)

(defn make-arena [w h]
  (vec (repeatedly w 
         (fn [] 
           (vec (repeatedly h 
                  #(ref nil)))))))

;; ou
(defn make-arena [w h]
  (let [arena (->>
                (repeatedly #(ref nil))
                (partition h)
                (map vec)
                (take w)
                vec)]
    (dosync 
      (doseq [i (range w)]
        (ref-set (get-in arena [i 0]) :wall)
        (ref-set (get-in arena [i (dec h)]) :wall))
      (doseq [j (range h)]
        (ref-set (get-in arena [0 j]) :wall)
        (ref-set (get-in arena [(dec w) j]) :wall)))
    arena))

(def arena (make-arena 10 10))

(defn print-arena [arena]
  (dosync
    (doseq [row arena]
      (let [vals (map deref row)
            chars (map 
                    #({:wall \X nil \space} % %) 
                    vals)]
        (println (apply str chars))))))

(defn stubborn-bot-factory
  "Crée un bot borné qui va toujours dans 
   la même direction"
  [[di dj]]
  {:strategy (fn [[i j] _]
               {:pos [(+ i di) (+ j dj)]
                :state nil})
   :state nil})

(def to-left
  {[0 1] [-1 0]
   [-1 0] [0 -1]
   [0 -1] [1 0]
   [1 0] [0 1]})

(defn avoider-bot-factory [[di dj :as dir]]
  {:strategy (fn [pos dir]
               (let [new-pos (map + pos dir)] 
                 (if @(get-in arena new-pos)
                   (let [new-dir (to-left dir)
                         new-pos (map + pos new-dir)]
                     {:pos new-pos :state new-dir})
                   {:pos new-pos :state dir})))
   :state dir} )

(defn play 
  "Execute la stratégie bot.
   Une strategie prend une position et renvoie une
   nouvelle position ou nil si impossible." 
  [name {bot :strategy init-state :state} init-pos]
  (loop [{pos :pos bot-state :state} 
           {:pos init-pos :state init-state}]
    (when pos
      (Thread/sleep 2000)
      (recur
        (dosync
          (let [r (get-in arena pos)] 
            (if @r
              (println name "argh!")
              (do 
                (ref-set r name)
                (bot pos bot-state)))))))))

