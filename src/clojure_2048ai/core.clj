(ns clojure-2048ai.core)

(def directions (sorted-map :left 0 :down 1 :right 2 :up 3))

(def blank-board
  (vec (repeat 4 (vec (repeat 4 0)))))

(defn blanks [board]
  (for [x (range 4)
        y (range 4)
        :when (zero? (get-in board [x y]))]
    [x y]))

(defn add-tile [board]
  (let [elem (rand-nth [2 2 2 2 4 2 2 2 2 2])
        pos  (rand-nth (blanks board))]
    (assoc-in board pos elem)))

(defn rotate [board]
  (apply mapv vector (reverse board)))

(defn rotate-board [n board]
  (nth (iterate rotate board) n))

(defn add-pairs [row]
  (->> row
       (remove zero?)
       (partition-by identity)
       (mapcat #(partition-all 2 %))
       (map #(apply + %))))

(defn padding [row]
  (take 4 (concat row (repeat 0))))

(defn move [board dir]
  (->> board
       (rotate-board (directions dir))
       (map add-pairs)
       (map padding)
       (rotate-board (- 4 (directions dir)))))

(defn init-game []
  (-> blank-board
      add-tile
      add-tile))

(defn print-board [board]
  (do
    (doseq [x (range 4)]
      (doseq [y (range 4)]
        (printf "%8d" (get-in board [x y])))
      (println))
    (println "--------------------------------")
    board))

(defn noop? [board dir]
  (= board (move board dir)))

(defn gameover? [board]
  (every? (partial noop? board) (keys directions)))

(defn max-val [board]
  (apply max (flatten board)))

(defn play [board dir]
  (-> board
      (move dir)
      (as-> new-board
            (if (= board new-board)
              new-board
              (add-tile new-board)))))

(defn auto-play [board player]
  (let [dir (player board)
        _   (prn dir)
        res (play board dir)]
    (print-board res)
    (if (gameover? res)
      (max-val board)
      (recur res player))))

(defn random-player [board]
  (rand-nth (keys directions)))


;;--------------AI-------------------------

(def coefs (take 16 (iterate #(* 0.25 %) 1)))

(defn val-pos [board]
  (for [row (range 4)]
    (for [col (range 4)]
      {:value (get-in board [row col]) :pos [row col]})))

(defn row-path [board]
  (concat (first board) (reverse (second board))
          (last (butlast board)) (reverse (last board))))

(defn col-path [board]
  (let [rev-board (apply mapv vector board)]
    (row-path rev-board)))

(defn calculate [path]
  (reduce + (map * (map :value path) coefs)))

(defn critical-blank [path]
  (:pos (first (filter (comp zero? :value) path))))

(defn evaluate [board]
  (let [v-p-board (val-pos board)
        boards    (take 4 (iterate rotate v-p-board))
        paths     (concat (map row-path boards) (map col-path boards))
        best-path (last (sort-by calculate paths))]
    {:score (calculate best-path) :pos (critical-blank best-path)}))

(defn noop? [board dir]
  (= board (move board dir)))

(defn next-move [board depth max-depth]
  (let [dirs (remove #(noop? board %) (keys directions))]
    (if (empty? dirs)
      {:dir :left :score 0 :pos [0 0]}
      (if (zero? depth)
        (->> dirs
             (map #(move board %))
             (map evaluate)
             (map #(assoc %2 :dir %1) dirs)
             (sort-by :score)
             last)
        (let [boards     (map #(move board %) dirs)
              scores-pos (map evaluate boards)
              scores     (map :score scores-pos)
              pos        (map :pos scores-pos)
              new-boards (map #(assoc-in %1 %2 2) boards pos)]
          (->> new-boards
               (map #(next-move % (dec depth) max-depth))
               (map #(+ %1 (/ (:score %2) (Math/pow 0.9 (+ (- max-depth depth) 1))))
                    scores)
               (map #(hash-map :pos %1 :score %2) pos)
               (map #(assoc %2 :dir %1) dirs)
               (sort-by :score)
               last))))))

(defn ai-player [board]
  (:dir (next-move board 3 3)))

(defn -main []
  (auto-play (init-game) ai-player))
